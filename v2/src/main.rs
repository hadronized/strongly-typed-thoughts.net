mod config;
mod file_store;

use std::fs;

use crate::config::Config;
use rocket::{get, launch, routes};

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

  // create the upload directory if missing
  fs::create_dir_all(user_config.upload_dir).unwrap();

  // file watchers; we watch for uploaded files and blog articles

  rocket::custom(rocket_config).mount("/", routes![index])
}

fn load_config() -> Config {
  fs::read_to_string("server.toml")
    .ok()
    .and_then(|contents| toml::from_str(&contents).ok())
    .unwrap_or_else(|| {
      eprintln!("fail to read configuration; using default");
      Config::default()
    })
}
