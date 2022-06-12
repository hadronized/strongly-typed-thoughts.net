use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::{
  collections::HashMap,
  fs::read_to_string,
  io,
  path::{Path, PathBuf},
};

#[derive(Debug)]
pub enum ArticleError {
  IOError(io::Error),
  SerdeError(serde_json::Error),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArticleIndex {
  articles: HashMap<String, ArticleMetadata>,
}

impl ArticleIndex {
  pub fn new() -> Self {
    Self {
      articles: HashMap::new(),
    }
  }

  pub fn articles(&self) -> &HashMap<String, ArticleMetadata> {
    &self.articles
  }

  pub fn populate_from_index(&mut self, path: &Path) -> Result<(), ArticleError> {
    let content = read_to_string(path)?;
    let articles: Vec<ArticleMetadata> = serde_json::from_str(&content)?;
    let articles = articles.into_iter().map(|a| (a.name.clone(), a)).collect();

    for (_, article) in &articles {
      log::info!("found blog article: {article:#?}");
    }

    self.articles = articles;

    Ok(())
  }
}
