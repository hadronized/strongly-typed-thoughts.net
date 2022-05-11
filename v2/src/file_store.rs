use magic::{Cookie, CookieFlags};
use mime::Mime;
use serde::{Deserialize, Serialize};
use std::{
  collections::HashSet,
  path::{Path, PathBuf},
};

pub struct FileManager {
  cookie: Cookie,
  index: FileIndex,
}

impl FileManager {
  pub fn new() -> Option<Self> {
    let cookie = Cookie::open(CookieFlags::default()).ok()?;
    let index = FileIndex::new();

    Some(Self { cookie, index })
  }

  pub fn add_or_update(&mut self, path: impl AsRef<Path>) -> Option<()> {
    let path = path.as_ref();
    self.mime_dispatch(path, |files| {
      files.insert(path.to_owned());
    })
  }

  pub fn remove(&mut self, path: impl AsRef<Path>) -> Option<()> {
    let path = path.as_ref();
    self.mime_dispatch(path, |files| {
      files.remove(path);
    })
  }

  fn mime_dispatch(
    &mut self,
    path: impl AsRef<Path>,
    f: impl FnOnce(&mut HashSet<PathBuf>),
  ) -> Option<()> {
    let path = path.as_ref();

    let mime: Mime = self
      .cookie
      .file(path)
      .ok()
      .and_then(|mime| mime.parse().ok())?;
    match mime.type_() {
      mime::IMAGE => f(&mut self.index.images),
      mime::APPLICATION => f(&mut self.index.applications),
      mime::VIDEO => f(&mut self.index.videos),
      mime::AUDIO => f(&mut self.index.audios),
      mime::PDF => f(&mut self.index.papers),
      mime::TEXT => f(&mut self.index.texts),
      _ => {
        log::warn!(
          "file {path} has an unsupported mime: {mime}",
          path = path.display()
        );

        f(&mut self.index.unknowns);
      }
    }

    Some(())
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FileIndex {
  images: HashSet<PathBuf>,
  applications: HashSet<PathBuf>,
  videos: HashSet<PathBuf>,
  audios: HashSet<PathBuf>,
  texts: HashSet<PathBuf>,
  papers: HashSet<PathBuf>,
  unknowns: HashSet<PathBuf>,
}

impl FileIndex {
  fn new() -> Self {
    Self {
      images: HashSet::new(),
      applications: HashSet::new(),
      videos: HashSet::new(),
      audios: HashSet::new(),
      texts: HashSet::new(),
      papers: HashSet::new(),
      unknowns: HashSet::new(),
    }
  }
}
