use std::path::Path;

use crate::{file_store::FileIndex, html_wrapper::html_wrap, state::State};
use rocket::{get, response::content::RawHtml};

#[get("/browse")]
pub fn browse_listing(state: &rocket::State<State>) -> RawHtml<String> {
  let html = state.cache().cache("/browse", || {
    let file_index = state.file_index().lock().expect("file index");
    html_wrap(render_browse_listing(&file_index))
  });

  RawHtml(html)
}

fn render_browse_listing(file_index: &FileIndex) -> String {
  let images: Vec<_> = file_index
    .images
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let applications: Vec<_> = file_index
    .applications
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let videos: Vec<_> = file_index
    .videos
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let audios: Vec<_> = file_index
    .audios
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let texts: Vec<_> = file_index
    .texts
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let papers: Vec<_> = file_index
    .papers
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();
  let unknowns: Vec<_> = file_index
    .unknowns
    .iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect();

  format!(
    include_str!("./browse.html"),
    images = images.join("\n"),
    applications = applications.join("\n"),
    videos = videos.join("\n"),
    audios = audios.join("\n"),
    texts = texts.join("\n"),
    papers = papers.join("\n"),
    unknowns = unknowns.join("\n"),
  )
}

fn path_to_html(i: usize, path: &Path) -> String {
  format!(
    include_str!("./browse_item.html"),
    oddity = if i & 1 == 1 { "odd" } else { "even" },
    path = path.display()
  )
}
