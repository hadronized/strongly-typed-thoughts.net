use rocket::{
  get,
  response::{
    content::RawHtml,
    status::{self, NotFound},
  },
  serde::json::Json,
};

use crate::{blog::ArticleMetadata, state::State};

#[get("/blog")]
pub fn api_blog(state: &rocket::State<State>) -> Json<Vec<ArticleMetadata>> {
  let index = state.blog_index().lock().expect("blog index");
  let articles = index
    .articles()
    .iter()
    .map(|(_, article)| article.metadata().clone())
    .collect();
  Json(articles)
}

#[get("/blog/<slug>")]
pub fn api_blog_article(
  state: &rocket::State<State>,
  slug: &str,
) -> Result<RawHtml<String>, NotFound<String>> {
  let mut index = state.blog_index().lock().expect("blog index");

  match index.articles_mut().get_mut(slug) {
    Some(article) => {
      let html = if let Some(html) = article.html() {
        RawHtml(html.clone())
      } else {
        log::info!("article {slug} not cached yet; caching…");

        let html = article.cache().map_err(|e| NotFound(e.to_string()))?;
        RawHtml(html)
      };

      Ok(html)
    }

    None => Err(status::NotFound(format!("article {slug} doesn’t exist"))),
  }
}
