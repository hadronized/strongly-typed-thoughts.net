use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(default)]
pub struct Config {
  pub port: u16,
  pub front_dir: PathBuf,
  pub media_dir: PathBuf,
  pub upload_dir: PathBuf,
  pub blog_index: PathBuf,
  pub gpg_key_file: PathBuf,
}

impl Default for Config {
  fn default() -> Self {
    Self {
      port: 8000,
      front_dir: "front".into(),
      media_dir: "media".into(),
      upload_dir: "media/uploads".into(),
      blog_index: "media/blog/index".into(),
      gpg_key_file: "media/gpg/phaazon.gpg".into(),
    }
  }
}
