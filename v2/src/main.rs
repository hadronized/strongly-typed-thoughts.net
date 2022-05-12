mod config;
mod file_store;
mod state;

use crate::{config::Config, state::State};
use notify::{DebouncedEvent, Watcher};
use rocket::{get, launch, log::LogLevel, routes};
use std::{
  fs,
  path::{Path, PathBuf},
  sync::{mpsc, Arc, Mutex},
  thread,
  time::Duration,
};

const NOTIFY_DEBOUNCE_DUR: Duration = Duration::from_millis(200);

#[get("/")]
fn index() -> &'static str {
  "Hello, world!"
}

#[launch]
fn rocket() -> _ {
  let user_config = load_config();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

  // shared state
  let state = Arc::new(Mutex::new(State::new()));

  // create the upload directory if missing
  fs::create_dir_all(&user_config.upload_dir).unwrap();

  // dispatch notify events
  {
    let state = state.clone();
    let canon_upload_dir = user_config
      .upload_dir
      .canonicalize()
      .expect("canonicalized upload dir");
    let canon_blog_dir = user_config
      .blog_index
      .parent()
      .expect("blog dir")
      .canonicalize()
      .expect("canonicalized blog dir");
    let _ = thread::spawn(move || {
      // file watchers; we watch for uploaded files and blog articles
      let (notify_sx, notify_rx) = mpsc::channel();
      let mut watcher = notify::watcher(notify_sx, NOTIFY_DEBOUNCE_DUR).expect("notify watcher");

      // watch media files
      if let Err(err) = watcher.watch(&canon_upload_dir, notify::RecursiveMode::NonRecursive) {
        log::error!(
          "cannot watch {upload_dir}: {err}",
          upload_dir = canon_upload_dir.display()
        );
      }

      log::info!(
        "watching media files at {upload_dir}",
        upload_dir = canon_upload_dir.display()
      );

      // TODO: watch blog articles
      if let Err(err) = watcher.watch(&canon_blog_dir, notify::RecursiveMode::NonRecursive) {
        log::error!(
          "cannot watch {blog_dir}: {err}",
          blog_dir = canon_blog_dir.display()
        );
      }

      log::info!(
        "watching blog directory at {blog_dir}",
        blog_dir = canon_blog_dir.display()
      );

      while let Ok(event) = notify_rx.recv() {
        match event {
          DebouncedEvent::Create(ref event_path) | DebouncedEvent::Write(ref event_path) => {
            if event_path.parent() == Some(&canon_upload_dir) {
              log::info!("uploaded file changed: {path}", path = event_path.display());
            }

            if event_path.parent() == Some(&canon_blog_dir) {
              log::info!("blog content changed");
            }
          }

          _ => (),
        }
      }
    });
  }

  rocket::custom(rocket_config).mount("/", routes![index])
}

fn load_config() -> Config {
  fs::read_to_string("server.toml")
    .ok()
    .and_then(|contents| toml::from_str(&contents).ok())
    .unwrap_or_else(|| {
      log::error!("fail to read configuration; using default");
      Config::default()
    })
}
