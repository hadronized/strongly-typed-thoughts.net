mod config;
mod file_store;
mod state;

use crate::{config::Config, state::State};
use notify::{DebouncedEvent, Watcher};
use rocket::{get, launch, log::LogLevel, routes};
use std::{
  fs,
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
    let _ = thread::spawn(move || {
      // file watchers; we watch for uploaded files and blog articles
      let (notify_sx, notify_rx) = mpsc::channel();
      let mut watcher = notify::watcher(notify_sx, NOTIFY_DEBOUNCE_DUR).expect("notify watcher");

      // watch media files
      if let Err(err) = watcher.watch(&user_config.upload_dir, notify::RecursiveMode::NonRecursive)
      {
        log::error!(
          "cannot watch {upload_dir}: {err}",
          upload_dir = user_config.upload_dir.display()
        );
      }

      log::info!(
        "watching media files at {upload_dir}",
        upload_dir = user_config.upload_dir.display()
      );

      // TODO: watch blog articles

      while let Ok(event) = notify_rx.recv() {
        match event {
          DebouncedEvent::Write(ref event_path) => {
            log::info!(
              "write event: {event:#?}; path: {event_path}",
              event_path = event_path.display()
            );
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
