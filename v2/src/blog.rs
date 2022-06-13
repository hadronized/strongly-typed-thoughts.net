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
  name: String,
  path: PathBuf,
  publish_date: DateTime<Utc>,
  modification_date: Option<DateTime<Utc>>,
  tags: Vec<String>,
  slug: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Article {
  metadata: ArticleMetadata,
  html: Option<String>,
}

impl Article {
  fn new(metadata: ArticleMetadata) -> Self {
    let html = None;
    Self { metadata, html }
  }

  pub fn metadata(&self) -> &ArticleMetadata {
    &self.metadata
  }

  pub fn html(&self) -> Option<&String> {
    self.html.as_ref()
  }

  pub fn cache(&mut self) -> Result<String, ArticleError> {
    let path = &self.metadata.path;
    let contents = read_to_string(path)?;
    let parser = pulldown_cmark::Parser::new(&contents);
    let mut html = String::new();
    pulldown_cmark::html::push_html(&mut html, parser);

    self.html = Some(html.clone());

    Ok(html)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArticleIndex {
  articles: HashMap<String, Article>,
}

impl ArticleIndex {
  pub fn new() -> Self {
    Self {
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
      .map(|a| (a.slug.clone(), Article::new(a)))
      .collect();

    for (_, article) in &articles {
      log::info!("found blog article: {article:#?}");
    }

    self.articles = articles;

    Ok(())
  }
}
