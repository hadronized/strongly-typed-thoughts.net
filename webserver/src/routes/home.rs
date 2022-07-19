use chrono::{Datelike as _, Utc};
use rocket::{get, response::content::RawHtml};

use crate::{html_wrapper::html_wrap, state::State};

#[get("/")]
pub fn home(state: &rocket::State<State>) -> RawHtml<String> {
  let html = state.cache().cache("/", || {
    let today = Utc::today();
    let age = today.year() - 1992;

    let html = format!(include_str!("./home.html"), age = age);
    html_wrap(html)
  });

  RawHtml(html)
}
