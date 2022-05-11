use crate::file_store::FileManager;

pub struct State {
  file_mgr: FileManager,
}

impl State {
  pub fn new() -> Option<Self> {
    Some(Self {
      file_mgr: FileManager::new()?,
    })
  }

  pub fn file_mgr(&mut self) -> &mut FileManager {
    &mut self.file_mgr
  }
}
