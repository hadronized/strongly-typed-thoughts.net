use magic::{flags::MIME_TYPE, Cookie, MagicError};
use mime::Mime;
use serde::{Deserialize, Serialize};
use std::{
  collections::HashSet,
  fmt,
  path::{Path, PathBuf},
  sync::{Arc, Mutex},
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FileError {
  MagicError(MagicError),
  MimeError(String),
}

impl fmt::Display for FileError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      FileError::MagicError(err) => write!(f, "magic error: {}", err),
      FileError::MimeError(err) => write!(f, "mime error: {}", err),
    }
  }
}

impl From<MagicError> for FileError {
  fn from(e: MagicError) -> Self {
    FileError::MagicError(e)
  }
}

pub struct FileManager {
  cookie: Cookie,
  index: Arc<Mutex<FileIndex>>,
}

impl FileManager {
  pub fn new(index: Arc<Mutex<FileIndex>>) -> Result<Self, FileError> {
    let cookie = Cookie::open(MIME_TYPE)?;
    cookie.load::<&str>(&[])?;

    Ok(Self { cookie, index })
  }

  pub fn populate_from_dir(&mut self, dir: impl AsRef<Path>) -> Result<(), FileError> {
    let dir = dir.as_ref();
    for entry in dir.read_dir().expect("read dir") {
      if let Ok(entry) = entry {
        self.add_or_update(entry.path())?;
      }
    }

    Ok(())
  }

  pub fn add_or_update(&mut self, path: impl AsRef<Path>) -> Result<(), FileError> {
    let path = path.as_ref();
    self.mime_dispatch(path, |files| {
      files.insert(path.to_owned());
    })
  }

  pub fn remove(&mut self, path: impl AsRef<Path>) -> Result<(), FileError> {
    let path = path.as_ref();
    self.mime_dispatch(path, |files| {
      files.remove(path);
    })
  }

  fn mime_dispatch(
    &mut self,
    path: impl AsRef<Path>,
    f: impl FnOnce(&mut HashSet<PathBuf>),
  ) -> Result<(), FileError> {
    let path = path.as_ref();

    let mime: Mime = self
      .cookie
      .file(path)?
      .parse()
      .map_err(|parse_err: mime::FromStrError| FileError::MimeError(parse_err.to_string()))?;
    let mut index = self.index.lock().expect("file index lock");
    match mime.type_() {
      mime::IMAGE => f(&mut index.images),
      mime::APPLICATION => f(&mut index.applications),
      mime::VIDEO => f(&mut index.videos),
      mime::AUDIO => f(&mut index.audios),
      mime::PDF => f(&mut index.papers),
      mime::TEXT => f(&mut index.texts),
      _ => {
        log::warn!(
          "file {path} has an unsupported mime: {mime}",
          path = path.display()
        );

        f(&mut index.unknowns);
      }
    }

    Ok(())
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
  pub fn new() -> Self {
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

  pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a Path> {
    self
      .images
      .iter()
      .chain(&self.applications)
      .chain(&self.videos)
      .chain(&self.audios)
      .chain(&self.texts)
      .chain(&self.papers)
      .chain(&self.unknowns)
      .map(|p| p.as_path())
  }
}
