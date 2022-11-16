use std::{
  collections::HashSet,
  path::{Path, PathBuf},
};

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

fn list_index_items(items: &HashSet<PathBuf>) -> Vec<String> {
  let mut ordered: Vec<_> = items.iter().collect();
  ordered.sort();

  ordered
    .into_iter()
    .enumerate()
    .map(|(i, path)| path_to_html(i, path))
    .collect()
}

fn render_browse_listing(file_index: &FileIndex) -> String {
  let images = list_index_items(&file_index.images);
  let applications = list_index_items(&file_index.applications);
  let videos = list_index_items(&file_index.videos);
  let audios = list_index_items(&file_index.audios);
  let texts = list_index_items(&file_index.texts);
  let papers = list_index_items(&file_index.papers);
  let unknowns = list_index_items(&file_index.unknowns);

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
