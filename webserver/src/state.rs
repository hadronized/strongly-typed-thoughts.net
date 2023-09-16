use crate::{
  blog::ArticleIndex,
  cache::Cache,
  config::Config,
  file_store::{FileIndex, FileManager},
};
use notify::{DebouncedEvent, Watcher};
use std::{
  path::Path,
  sync::{
    mpsc::{self, Receiver},
    Arc, Mutex,
  },
  thread,
  time::Duration,
};

const NOTIFY_DEBOUNCE_DUR: Duration = Duration::from_millis(200);

pub struct State {
  cache: Cache,
  file_index: Arc<Mutex<FileIndex>>,
  blog_index: Arc<Mutex<ArticleIndex>>,
}

impl State {
  pub fn new(config: &Config) -> Option<Self> {
    let cache = Cache::default();
    cache.schedule_eviction();

    Some(Self {
      cache,
      file_index: Arc::new(Mutex::new(FileIndex::new())),
      blog_index: Arc::new(Mutex::new(ArticleIndex::new(&config.blog_dir))),
    })
  }

  pub fn cache(&self) -> &Cache {
    &self.cache
  }

  /// Spawn a new thread to start watching files in (via `notify`).
  pub fn spawn_and_watch_files(
    &mut self,
    cache: Cache,
    has_launched_rx: Receiver<()>,
    config: Config,
  ) {
    let canon_upload_dir = config
      .upload_dir
      .canonicalize()
      .expect("canonicalized upload dir");
    let canon_blog_dir = config
      .blog_dir
      .canonicalize()
      .expect("canonicalized blog dir");
    let blog_index_path = canon_blog_dir.join("index.json");
    let file_index = self.file_index().clone();
    let blog_index = self.blog_index().clone();

    let _ = thread::spawn(move || {
      has_launched_rx
        .recv_timeout(Duration::from_secs(5))
        .expect("rocket has launched");

      log::debug!("{config:#?}");

      // file watchers; we watch for uploaded files and blog articles
      let (notify_sx, notify_rx) = mpsc::channel();
      let mut watcher = notify::watcher(notify_sx, NOTIFY_DEBOUNCE_DUR).expect("notify watcher");

      Self::watch_dir("media files", &canon_upload_dir, &mut watcher);
      Self::watch_dir("blog directory", &canon_blog_dir, &mut watcher);
      Self::watch_loop(
        notify_rx,
        &canon_upload_dir,
        &canon_blog_dir,
        &blog_index_path,
        file_index,
        blog_index,
        cache,
      );
    });
  }

  /// Watch a specific directory.
  fn watch_dir(kind: &'static str, dir: &Path, watcher: &mut impl Watcher) {
    if let Err(err) = watcher.watch(dir, notify::RecursiveMode::NonRecursive) {
      log::error!("cannot watch {dir}: {err}", dir = dir.display());
    }

    log::info!("watching {kind} at {dir}", dir = dir.display());
  }

  /// Main watch logic.
  fn watch_loop(
    notify_rx: Receiver<DebouncedEvent>,
    upload_dir: &Path,
    blog_dir: &Path,
    blog_index_path: &Path,
    file_index: Arc<Mutex<FileIndex>>,
    blog_index: Arc<Mutex<ArticleIndex>>,
    cache: Cache,
  ) {
    let mut file_mgr = FileManager::new(file_index).expect("file manager");

    if let Err(err) = file_mgr.populate_from_dir(upload_dir) {
      log::error!("cannot populate uploads while startup: {err}",);
    }

    if let Err(err) = blog_index
      .lock()
      .expect("blog index")
      .populate_from_index(blog_index_path)
    {
      log::error!("cannot populate blog index while startup: {err}");
    }

    while let Ok(event) = notify_rx.recv() {
      match event {
        DebouncedEvent::Create(ref event_path) | DebouncedEvent::Write(ref event_path) => {
          if event_path.parent() == Some(upload_dir) {
            log::info!("uploaded file changed: {path}", path = event_path.display());

            cache.invalidate_all();

            if let Err(err) = file_mgr.add_or_update(event_path) {
              log::error!(
                "cannot add or update file {file}: {err}",
                file = event_path.display()
              );
            }
          } else if event_path.parent() == Some(blog_dir) {
            log::info!("blog content changed; {:?}", event_path.file_name());

            cache.invalidate_all();

            if let Err(err) = blog_index
              .lock()
              .expect("blog index")
              .populate_from_index(blog_index_path)
            {
              log::error!("error while updating blog content: {}", err);
            }
          }
        }

        DebouncedEvent::Remove(ref event_path) => {
          if event_path.parent() == Some(upload_dir) {
            log::info!(
              "removing uploaded file: {path}",
              path = event_path.display()
            );

            file_mgr.remove(event_path);
          }
        }

        _ => (),
      }
    }
  }

  pub fn file_index(&self) -> &Arc<Mutex<FileIndex>> {
    &self.file_index
  }

  pub fn blog_index(&self) -> &Arc<Mutex<ArticleIndex>> {
    &self.blog_index
  }
}
