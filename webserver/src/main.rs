mod api;
mod blog;
mod config;
mod file_store;
mod state;

use crate::{
  api::{
    blog::{api_blog, api_blog_article},
    browse::api_browse,
  },
  config::Config,
  state::State,
};
use rocket::{
  config::TlsConfig,
  fs::{FileServer, Options},
  get, launch,
  log::LogLevel,
  response::content::RawXml,
  routes,
};
use std::fs;

#[get("/feed")]
pub fn blog_feed(state: &rocket::State<State>) -> RawXml<String> {
  let index = state.blog_index().lock().expect("blog index");
  let channel = index.to_rss();
  RawXml(channel.to_string())
}

#[launch]
fn rocket() -> _ {
  let user_config = Config::load();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

  if let (Some(tls_cert), Some(tls_cert_key)) = (&user_config.tls_cert, &user_config.tls_cert_key) {
    log::info!("enabling TLS");
    TlsConfig::from_paths(tls_cert, tls_cert_key);
  }

  // state
  let mut state = State::new().expect("state");

  // create the upload directory if missing
  fs::create_dir_all(&user_config.upload_dir).unwrap();

  state.spawn_and_watch_files(&user_config);

  let index = FileServer::new("static", Options::default()).rank(0);
  let static_files = FileServer::new("static", Options::default()).rank(1);
  let media_uploads = FileServer::new("media/uploads", Options::default());

  rocket::custom(rocket_config)
    .mount("/", index)
    .mount("/static", static_files)
    .mount("/media/uploads", media_uploads)
    .mount("/blog", routes![blog_feed])
    .mount("/api", routes![api_browse, api_blog, api_blog_article])
    .manage(state)
}
