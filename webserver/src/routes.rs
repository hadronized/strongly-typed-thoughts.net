pub mod blog;
pub mod browse;
pub mod home;

use rocket::{routes, Route};

pub fn routes() -> Vec<Route> {
  routes![
    home::home,
    blog::blog_listing,
    blog::blog_article,
    blog::blog_feed,
    browse::browse_listing,
  ]
}
