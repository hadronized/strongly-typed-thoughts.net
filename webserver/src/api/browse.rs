use rocket::{get, serde::json::Json};

use crate::{file_store::FileIndex, state::State};

#[get("/browse")]
pub fn api_browse(state: &rocket::State<State>) -> Json<FileIndex> {
  let index = state.file_index().lock().expect("file index");
  Json(index.clone())
}
