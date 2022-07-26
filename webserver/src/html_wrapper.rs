use std::fmt::Display;

use chrono::{Datelike as _, Utc};

pub fn html_wrap(contents: impl Display) -> String {
  let year = Utc::now().year();
  format!(
    include_str!("html_wrapper.html"),
    contents = contents,
    year = year
  )
}
