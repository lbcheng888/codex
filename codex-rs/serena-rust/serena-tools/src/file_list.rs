use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::path::Path;
use tokio::fs;
use serena_core::tools::{Tool, ToolContext};

#[derive(Debug, Deserialize)]
pub struct FileListInput {
    /// Base path to list
    pub path: String,
    /// Whether to list recursively (default: false)
    #[serde(default)]
    pub recursive: bool,
    /// Optional glob pattern to filter (matched against entry relative path)
    #[serde(default)]
    pub pattern: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct FileListEntry {
    pub path: String,
    pub is_dir: bool,
}

#[derive(Debug, Serialize)]
pub struct FileListOutput {
    pub entries: Vec<FileListEntry>,
}

pub struct FileList;

#[async_trait]
impl Tool for FileList {
    fn name(&self) -> &'static str { "file_list" }
    fn description(&self) -> &'static str { "List files/directories under a path (optionally recursive with glob pattern)" }

    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let input: FileListInput = serde_json::from_value(input)?;
        let base = ctx.cfg.workspace_root.join(&input.path);
        if !base.exists() {
            return Err(serena_core::error::CoreError::validation(format!("Path does not exist: {}", base.display())));
        }
        if !base.is_dir() {
            return Err(serena_core::error::CoreError::validation(format!("Path is not a directory: {}", base.display())));
        }

        let pat_string = input.pattern.clone();

        let mut entries: Vec<FileListEntry> = Vec::new();
        if input.recursive {
            let walker = walkdir::WalkDir::new(&base).into_iter();
            for e in walker.filter_map(|e| e.ok()) {
                if e.path() == base { continue; }
                let rel = path_relative(&base, e.path());
                let is_dir = e.file_type().is_dir();
                if let Some(pat) = &pat_string {
                    if !glob_match(pat, &rel) { continue; }
                }
                entries.push(FileListEntry { path: rel, is_dir });
            }
        } else {
            let mut rd = fs::read_dir(&base).await?;
            while let Some(de) = rd.next_entry().await? {
                let p = de.path();
                let is_dir = de.file_type().await.map(|t| t.is_dir()).unwrap_or(false);
                let rel = path_relative(&base, &p);
                if let Some(pat) = &pat_string {
                    if !glob_match(pat, &rel) { continue; }
                }
                entries.push(FileListEntry { path: rel, is_dir });
            }
        }
        entries.sort_by(|a,b| a.path.cmp(&b.path));
        Ok(json!(FileListOutput { entries }))
    }
}

fn path_relative(base: &Path, p: &Path) -> String {
    match p.strip_prefix(base) {
        Ok(rel) => rel.to_string_lossy().to_string(),
        Err(_) => p.to_string_lossy().to_string(),
    }
}

fn glob_match(pattern: &str, rel_path: &str) -> bool {
    // Build a matcher each time (acceptable for our size). Use globset for correct semantics.
    let glob = match globset::Glob::new(pattern) { Ok(g) => g, Err(_) => return false };
    let mut builder = globset::GlobSetBuilder::new();
    builder.add(glob);
    let set = match builder.build() { Ok(s) => s, Err(_) => return false };
    set.is_match(rel_path)
}
