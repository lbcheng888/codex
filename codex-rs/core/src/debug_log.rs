#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*) => {
        if std::env::var("CODEX_DEBUG").is_ok() {
            let msg = format!($($arg)*);
            let timestamp = chrono::Utc::now().format("%Y-%m-%d %H:%M:%S%.3f");
            let full_msg = format!("[{}] {}", timestamp, msg);
            
            // Always print to stderr
            eprintln!("{}", full_msg);
            
            // Also try to write to debug file
            if let Ok(home) = std::env::var("HOME") {
                let log_path = std::path::PathBuf::from(home).join(".codex").join("debug.log");
                if let Some(parent) = log_path.parent() {
                    let _ = std::fs::create_dir_all(parent);
                }
                
                if let Ok(mut file) = std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&log_path) 
                {
                    use std::io::Write;
                    let _ = writeln!(file, "{}", full_msg);
                    let _ = file.flush();
                }
            }
        }
    };
}