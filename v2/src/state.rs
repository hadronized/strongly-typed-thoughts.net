use crate::{blog::ArticleIndex, file_store::FileIndex};
use std::sync::{Arc, Mutex};

pub struct State {
  file_index: Arc<Mutex<FileIndex>>,
  blog_index: Arc<Mutex<ArticleIndex>>,
}

impl State {
  pub fn new() -> Option<Self> {
    Some(Self {
      file_index: Arc::new(Mutex::new(FileIndex::new())),
      blog_index: Arc::new(Mutex::new(ArticleIndex::new())),
    })
  }

  pub fn file_index(&self) -> &Arc<Mutex<FileIndex>> {
    &self.file_index
  }

  pub fn blog_index(&self) -> &Arc<Mutex<ArticleIndex>> {
    &self.blog_index
  }
}
