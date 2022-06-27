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
  response::content::{RawHtml, RawXml},
  routes,
};
use std::{
  fs,
  net::{IpAddr, Ipv4Addr},
};

#[get("/feed")]
pub fn blog_feed(state: &rocket::State<State>) -> RawXml<String> {
  let index = state.blog_index().lock().expect("blog index");
  let channel = index.to_rss();
  RawXml(channel.to_string())
}

#[get("/<article_slug>")]
pub fn blog_article<'a>(article_slug: &str, state: &'a rocket::State<State>) -> RawHtml<&'a str> {
  let _ = article_slug;
  let index_html = state.index_html();
  RawHtml(index_html)
}

#[launch]
fn rocket() -> _ {
  let user_config = Config::load();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

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

  state.spawn_and_watch_files(&user_config);

  let index = FileServer::new(&user_config.static_dir, Options::default()).rank(0);
  let static_files = FileServer::new(&user_config.static_dir, Options::default()).rank(1);
  let media_uploads = FileServer::new(&user_config.upload_dir, Options::default());

  rocket::custom(rocket_config)
    .mount("/", index)
    .mount("/static", static_files)
    .mount("/media/uploads", media_uploads)
    .mount("/blog", routes![blog_feed, blog_article])
    .mount("/api", routes![api_browse, api_blog, api_blog_article])
    .manage(state)
}
