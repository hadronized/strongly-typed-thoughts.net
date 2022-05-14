mod config;
mod file_store;
mod state;

use crate::{
  config::Config,
  state::{SharedState, State},
};
use notify::{DebouncedEvent, Watcher};
use rocket::{get, launch, log::LogLevel, routes};
use std::{
  fs,
  path::Path,
  sync::{
    mpsc::{self, Receiver},
    Arc, Mutex,
  },
  thread,
  time::Duration,
};

const NOTIFY_DEBOUNCE_DUR: Duration = Duration::from_millis(200);

#[get("/")]
fn index() -> &'static str {
  "Hello, world!"
}

#[get("/media/uploads")]
fn list_uploads(state: SharedState) ->

#[launch]
fn rocket() -> _ {
  let user_config = load_config();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

  // shared state
  let state = Arc::new(Mutex::new(State::new().expect("state")));

  // create the upload directory if missing
  fs::create_dir_all(&user_config.upload_dir).unwrap();

  spawn_and_watch_files(&user_config, state);

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

/// Spawn a new thread to start watching files in (via `notify`).
fn spawn_and_watch_files(config: &Config, state: SharedState) {
  let canon_upload_dir = config
    .upload_dir
    .canonicalize()
    .expect("canonicalized upload dir");
  let canon_blog_dir = config
    .blog_index
    .parent()
    .expect("blog dir")
    .canonicalize()
    .expect("canonicalized blog dir");

  let _ = thread::spawn(move || {
    // file watchers; we watch for uploaded files and blog articles
    let (notify_sx, notify_rx) = mpsc::channel();
    let mut watcher = notify::watcher(notify_sx, NOTIFY_DEBOUNCE_DUR).expect("notify watcher");

    watch_dir("media files", &canon_upload_dir, &mut watcher);
    watch_dir("blog directory", &canon_blog_dir, &mut watcher);

    watch_loop(notify_rx, &canon_upload_dir, &canon_blog_dir, state);
  });
}

/// Watch a specific directory.
fn watch_dir(kind: &'static str, dir: &Path, watcher: &mut impl Watcher) {
  if let Err(err) = watcher.watch(dir, notify::RecursiveMode::NonRecursive) {
    log::error!("cannot watch {dir}: {err}", dir = dir.display());
  }

  log::info!("watching {kind} at {dir}", dir = dir.display());
}

/// Main watch logic.
fn watch_loop(
  notify_rx: Receiver<DebouncedEvent>,
  upload_dir: &Path,
  blog_dir: &Path,
  state: SharedState,
) {
  while let Ok(event) = notify_rx.recv() {
    match event {
      DebouncedEvent::Create(ref event_path) | DebouncedEvent::Write(ref event_path) => {
        if event_path.parent() == Some(upload_dir) {
          log::info!("uploaded file changed: {path}", path = event_path.display());
          add_or_update_file(&state, &event_path);
        }

        if event_path.parent() == Some(blog_dir) {
          log::info!("blog content changed");
        }
      }

      _ => (),
    }
  }
}

/// Add a new file to be tracked in the file index.
fn add_or_update_file(state: &SharedState, path: &Path) {
  let mut state = state.lock().expect("track_file lock");

  let _ = state.file_mgr().add_or_update(path);
}
