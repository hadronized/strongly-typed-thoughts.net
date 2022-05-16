use crate::file_store::FileIndex;
use std::sync::{Arc, Mutex};

pub struct State {
  file_index: Arc<Mutex<FileIndex>>,
}

impl State {
  pub fn new() -> Option<Self> {
    Some(Self {
      file_index: Arc::new(Mutex::new(FileIndex::new())),
    })
  }

  pub fn file_index(&self) -> &Arc<Mutex<FileIndex>> {
    &self.file_index
  }
}
