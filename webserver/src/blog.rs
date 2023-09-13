use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::{
  collections::HashMap,
  fmt::{self, Display},
  fs::read_to_string,
  io,
  path::{Path, PathBuf},
};

#[derive(Debug)]
pub enum ArticleError {
  IOError(io::Error),
  SerdeError(serde_json::Error),
}

impl Display for ArticleError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
    match self {
      ArticleError::IOError(e) => write!(f, "IO error: {}", e),
      ArticleError::SerdeError(e) => write!(f, "deserialization error: {}", e),
    }
  }
}

impl From<io::Error> for ArticleError {
  fn from(e: io::Error) -> Self {
    ArticleError::IOError(e)
  }
}

impl From<serde_json::Error> for ArticleError {
  fn from(e: serde_json::Error) -> Self {
    ArticleError::SerdeError(e)
  }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ArticleMetadata {
  pub name: String,
  pub path: PathBuf,
  pub publish_date: DateTime<Utc>,
  pub modification_date: Option<DateTime<Utc>>,
  pub tags: Vec<String>,
  pub slug: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Article {
  metadata: ArticleMetadata,
  html: String,
}

impl Article {
  fn new(blog_dir: &Path, metadata: ArticleMetadata) -> Result<Self, ArticleError> {
    let html = Self::load(blog_dir, &metadata.path)?;
    Ok(Self { metadata, html })
  }

  pub fn metadata(&self) -> &ArticleMetadata {
    &self.metadata
  }

  pub fn html(&self) -> &str {
    &self.html
  }

  pub fn load(blog_dir: &Path, article_path: &Path) -> Result<String, ArticleError> {
    let path = blog_dir.join(article_path);
    let contents = read_to_string(path)?;
    let parser = pulldown_cmark::Parser::new(&contents);
    let mut html = String::new();
    pulldown_cmark::html::push_html(&mut html, parser);

    Ok(html)
  }

  pub fn to_rss(&self) -> rss::Item {
    let date = self
      .metadata
      .modification_date
      .as_ref()
      .unwrap_or(&self.metadata.publish_date);

    rss::ItemBuilder::default()
      .author(Some(
        "Dimitri 'phaazon' Sabadie <dimitri.sabadie@gmail.com>".to_owned(),
      ))
      .pub_date(Some(format!(
        "{}",
        date.format("%a, %d %b %Y %H:%M:%S GMT")
      )))
      .link(Some(format!(
        "https://phaazon.net/blog/{}",
        self.metadata.slug
      )))
      .title(Some(self.metadata.name.clone()))
      .description(Some(self.html.clone()))
      .build()
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArticleIndex {
  blog_dir: PathBuf,
  articles: HashMap<String, Article>,
}

impl ArticleIndex {
  pub fn new(blog_dir: impl Into<PathBuf>) -> Self {
    let blog_dir = blog_dir.into();

    Self {
      blog_dir,
      articles: HashMap::new(),
    }
  }

  pub fn articles(&self) -> &HashMap<String, Article> {
    &self.articles
  }

  pub fn articles_mut(&mut self) -> &mut HashMap<String, Article> {
    &mut self.articles
  }

  pub fn populate_from_index(&mut self, path: &Path) -> Result<(), ArticleError> {
    let content = read_to_string(path)?;
    let articles: Vec<ArticleMetadata> = serde_json::from_str(&content)?;
    let articles = articles
      .into_iter()
      .flat_map(|a| {
        let slug = a.slug.clone();
        match Article::new(&self.blog_dir, a) {
          Ok(article) => Some((slug, article)),
          Err(err) => {
            log::error!("cannot load article {}: {}", slug, err);
            None
          }
        }
      })
      .collect();

    self.articles = articles;

    Ok(())
  }

  pub fn to_rss(&self) -> rss::Channel {
    let items: Vec<rss::Item> = self.articles.values().map(Article::to_rss).collect();
    let last_build_date = self
      .articles
      .values()
      .map(|art| {
        let m = &art.metadata;
        m.modification_date.as_ref().unwrap_or(&m.publish_date)
      })
      .max()
      .map(|date| date.format("%a, %d %b %Y %H:%M:%S GMT").to_string());

    rss::ChannelBuilder::default()
      .title("phaazon.net blog".to_owned())
      .link("https://phaazon.net/blog".to_owned())
      .items(items)
      .last_build_date(last_build_date)
      .build()
  }
}
