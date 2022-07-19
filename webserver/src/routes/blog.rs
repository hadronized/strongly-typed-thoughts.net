use std::{cmp::Reverse, fmt::Display};

use chrono::{DateTime, Utc};
use rocket::{
  get,
  response::{
    content::{RawHtml, RawXml},
    status::NotFound,
  },
};

use crate::{blog::Article, html_wrapper::html_wrap, state::State};

#[get("/blog")]
pub fn blog_listing(state: &rocket::State<State>) -> RawHtml<String> {
  let html = state.cache().cache("/blog", || {
    let index = state.blog_index().lock().expect("blog index");
    let mut articles: Vec<_> = index.articles().values().collect();
    articles.sort_by_key(|a| Reverse(a.metadata().publish_date));

    let article_list: Vec<_> = articles.into_iter().map(article_html_preview).collect();

    let html = format!(
      include_str!("./blog.html"),
      article_list = article_list.join("\n")
    );

    html_wrap(html)
  });

  RawHtml(html)
}

#[get("/blog/<slug>")]
pub fn blog_article(
  slug: &str,
  state: &rocket::State<State>,
) -> Result<RawHtml<String>, NotFound<String>> {
  state
    .cache()
    .cache_or_error(format!("/blog/{}", slug), || {
      let mut index = state.blog_index().lock().expect("blog index");
      let article = index
        .articles_mut()
        .get_mut(slug)
        .ok_or_else(|| NotFound(format!("article {} not found", slug)))?;

      Ok(article_html(article))
    })
    .map(RawHtml)
}

#[get("/blog/feed")]
pub fn blog_feed(state: &rocket::State<State>) -> RawXml<String> {
  RawXml(state.cache().cache("/blog/feed", || {
    let index = state.blog_index().lock().expect("blog index");
    let channel = index.to_rss();
    channel.to_string()
  }))
}

fn publication_date_html(date: &DateTime<Utc>) -> impl Display {
  date.format("%c %Z")
}

fn article_html_preview(article: &Article) -> String {
  let m = article.metadata();
  format!(
    include_str!("./blog_article_preview.html"),
    slug = m.slug,
    name = m.name,
    publish_date = publication_date_html(&m.publish_date)
  )
}

fn article_html(article: &mut Article) -> String {
  let m = article.metadata();
  let slug = m.slug.clone();
  let name = m.name.clone();
  let tags = m.tags.join(", ");
  let publication_date = publication_date_html(&m.publish_date);
  let contents = article.html();

  let html = format!(
    include_str!("./blog_article.html"),
    slug = slug,
    name = name,
    tags = tags,
    publication_date = publication_date,
    contents = contents,
  );

  html_wrap(html)
}
