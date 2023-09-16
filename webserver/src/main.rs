mod blog;
mod cache;
mod config;
mod file_store;
mod html_wrapper;
mod routes;
mod state;

use crate::{config::Config, state::State};
use rocket::{
  config::TlsConfig,
  fairing::AdHoc,
  fs::{FileServer, Options},
  launch,
  log::LogLevel,
};
use std::{
  fs,
  net::{IpAddr, Ipv4Addr},
  sync::mpsc,
};

#[launch]
fn rocket() -> _ {
  let user_config = Config::load();

  let mut rocket_config = rocket::Config {
    port: user_config.port,
    log_level: LogLevel::Debug,
    ..rocket::Config::default()
  };

  if !cfg!(debug_assertions) {
    rocket_config.address = IpAddr::V4(Ipv4Addr::UNSPECIFIED);
  }

  if let (Some(tls_cert), Some(tls_cert_key)) = (&user_config.tls_cert, &user_config.tls_cert_key) {
    log::info!("enabling TLS");
    TlsConfig::from_paths(tls_cert, tls_cert_key);
  }

  // state
  let mut state = State::new(&user_config).expect("state");

  // create the upload directory if missing
  fs::create_dir_all(&user_config.upload_dir).unwrap();

  // synchronization between the state runner and rocket
  let (has_launched_sx, has_launched_rx) = mpsc::sync_channel(0);

  let static_files = FileServer::new(&user_config.static_dir, Options::default());
  let media_uploads = FileServer::new(&user_config.upload_dir, Options::default());

  state.spawn_and_watch_files(state.cache().clone(), has_launched_rx, user_config);

  rocket::custom(rocket_config)
    .mount("/", routes::routes())
    .mount("/static", static_files)
    .mount("/media/uploads", media_uploads)
    .attach(AdHoc::on_liftoff("state sync", |_| {
      Box::pin(async move {
        has_launched_sx.send(()).expect("rocket has launched");
      })
    }))
    .manage(state)
}
