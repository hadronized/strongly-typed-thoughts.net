mod config;
mod file_store;
mod state;

use crate::{
  config::Config,
  file_store::{FileIndex, FileManager},
  state::State,
};
use notify::{DebouncedEvent, Watcher};
use rocket::{get, launch, log::LogLevel, routes, serde::json::Json};
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
fn list_uploads(state: &rocket::State<State>) -> Json<Vec<String>> {
  let uploads = state
    .file_index()
    .lock()
    .expect("file index")
    .iter()
    .map(|path| path.to_str().unwrap_or("").to_owned())
    .collect();
  Json(uploads)
}

#[launch]
fn rocket() -> _ {
  let user_config = load_config();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

  // state
  let mut state = State::new().expect("state");

  // create the upload directory if missing
  fs::create_dir_all(&user_config.upload_dir).unwrap();

  spawn_and_watch_files(&user_config, &mut state);

  rocket::custom(rocket_config)
    .mount("/", routes![index, list_uploads])
    .manage(state)
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
fn spawn_and_watch_files(config: &Config, state: &mut State) {
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
  let file_index = state.file_index().clone();

  let _ = thread::spawn(move || {
    // file watchers; we watch for uploaded files and blog articles
    let (notify_sx, notify_rx) = mpsc::channel();
    let mut watcher = notify::watcher(notify_sx, NOTIFY_DEBOUNCE_DUR).expect("notify watcher");

    watch_dir("media files", &canon_upload_dir, &mut watcher);
    watch_dir("blog directory", &canon_blog_dir, &mut watcher);
    watch_loop(notify_rx, &canon_upload_dir, &canon_blog_dir, file_index);
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
  file_index: Arc<Mutex<FileIndex>>,
) {
  let mut file_mgr = FileManager::new(file_index).expect("file manager");
  file_mgr
    .populate_from_dir(upload_dir)
    .expect("populate uploads");

  while let Ok(event) = notify_rx.recv() {
    match event {
      DebouncedEvent::Create(ref event_path) | DebouncedEvent::Write(ref event_path) => {
        if event_path.parent() == Some(upload_dir) {
          log::info!("uploaded file changed: {path}", path = event_path.display());

          if let Err(err) = file_mgr.add_or_update(&event_path) {
            log::error!(
              "cannot add or update file {file}: {err}",
              file = event_path.display()
            );
          }
        }

        if event_path.parent() == Some(blog_dir) {
          log::info!("blog content changed");
        }
      }

      DebouncedEvent::Remove(ref event_path) => {
        if event_path.parent() == Some(upload_dir) {
          log::info!(
            "removing uploaded file: {path}",
            path = event_path.display()
          );

          file_mgr.remove(&event_path);
        }
      }

      _ => (),
    }
  }
}
