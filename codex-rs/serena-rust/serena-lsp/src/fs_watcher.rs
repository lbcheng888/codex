use notify::{RecommendedWatcher, RecursiveMode, Watcher, Event, Config as NotifyConfig};
use std::path::PathBuf;
use std::sync::mpsc::{channel, Sender};
use std::thread;

/// 文件系统变更事件的精简封装
#[derive(Debug, Clone)]
pub enum FsChangeKind { Created, Modified, Removed, Renamed }

#[derive(Debug, Clone)]
pub struct FsChange { pub path: PathBuf, pub kind: FsChangeKind }

/// 跨平台文件监听（macOS 使用 FSEvents 后端，由 notify 自动选择）
pub struct FileWatcher {
    _handle: thread::JoinHandle<()>,
}

impl FileWatcher {
    /// 启动监听，将事件转发到给定 sender（非阻塞）
    pub fn start(paths: Vec<PathBuf>, out: Sender<FsChange>) -> notify::Result<Self> {
        let handle = thread::spawn(move || {
            let (tx, rx) = channel();
            let mut watcher = RecommendedWatcher::new(tx, NotifyConfig::default()).expect("create watcher");
            for p in &paths {
                let _ = watcher.watch(p, RecursiveMode::Recursive);
            }
            while let Ok(event) = rx.recv() {
                if let Ok(ev) = event {
                    if let Some(change) = map_event(ev) { let _ = out.send(change); }
                }
            }
        });
        Ok(Self { _handle: handle })
    }
}

fn map_event(e: Event) -> Option<FsChange> {
    use notify::event::EventKind;
    let kind = match e.kind {
        EventKind::Create(_) => FsChangeKind::Created,
        EventKind::Modify(_) => FsChangeKind::Modified,
        EventKind::Remove(_) => FsChangeKind::Removed,
        EventKind::Access(_) => FsChangeKind::Modified,
        EventKind::Any => return None,
        EventKind::Other => FsChangeKind::Modified,
    };
    let path = e.paths.first()?.clone();
    Some(FsChange { path, kind })
}
