//! Typed HTML symbols and functions.

use rocket::{http::ContentType, response::Responder};

#[derive(Responder)]
#[response(content_type = "html")]
pub struct Html(String);

impl Html {
  pub fn new(content: impl Into<String>) -> Self {
    Self(content.into())
  }
}

/// Wrapper for any page.
pub fn page_wrap() -> Html {
  let content = r#"{
    <div>
      <nav id="top-header" class="hero is-medium">
      </nav>
    </div>
  }"#;

  Html::new(content)
}
