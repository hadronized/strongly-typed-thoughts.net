use std::{fs, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(default)]
pub struct Config {
  pub port: u16,
  pub static_dir: PathBuf,
  pub front_dir: PathBuf,
  pub upload_dir: PathBuf,
  pub blog_index: PathBuf,
  pub tls_cert: Option<PathBuf>,
  pub tls_cert_key: Option<PathBuf>,
}

impl Config {
  pub fn load() -> Self {
    fs::read_to_string("server.toml")
      .ok()
      .and_then(|contents| toml::from_str(&contents).ok())
      .unwrap_or_else(|| {
        log::error!("fail to read configuration; using default");
        Config::default()
      })
  }
}

impl Default for Config {
  fn default() -> Self {
    Self {
      port: 8000,
      static_dir: "static".into(),
      front_dir: "frontend".into(),
      upload_dir: "media/uploads".into(),
      blog_index: "media/blog/index.json".into(),
      tls_cert: None,
      tls_cert_key: None,
    }
  }
}
