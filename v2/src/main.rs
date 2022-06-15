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
  fs::{FileServer, Options},
  launch,
  log::LogLevel,
  routes,
};
use std::fs;

#[launch]
fn rocket() -> _ {
  let user_config = Config::load();
  println!("{user_config:#?}");

  let mut rocket_config = rocket::Config::default();
  rocket_config.port = user_config.port;
  rocket_config.log_level = LogLevel::Debug;

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
    .mount("/api", routes![api_browse, api_blog, api_blog_article])
    .manage(state)
}
