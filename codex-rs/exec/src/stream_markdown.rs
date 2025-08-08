use owo_colors::OwoColorize;

/// Simple streaming Markdown renderer focused on fenced code blocks (```).
/// Accumulates partial lines; formats completed lines as they arrive.
pub struct StreamingMd {
    enable_ansi: bool,
    in_code_block: bool,
    partial: String,
}

impl StreamingMd {
    pub fn new(enable_ansi: bool) -> Self {
        Self {
            enable_ansi,
            in_code_block: false,
            partial: String::new(),
        }
    }

    pub fn reset(&mut self) {
        self.in_code_block = false;
        self.partial.clear();
    }

    /// Accept a delta chunk and return a formatted string to print.
    pub fn write(&mut self, delta: &str) -> String {
        let mut out = String::new();
        for ch in delta.chars() {
            if ch == '\n' {
                let line = std::mem::take(&mut self.partial);
                out.push_str(&self.format_line(line.as_str()));
                out.push('\n');
            } else {
                self.partial.push(ch);
            }
        }
        out
    }

    fn format_line(&mut self, line: &str) -> String {
        // Toggle fenced code block when encountering a line starting with ```
        let trimmed = line.trim_start();
        if trimmed.starts_with("```") {
            // toggle and do not emit the fence line itself
            self.in_code_block = !self.in_code_block;
            return String::new();
        }

        if self.in_code_block {
            let mut s = String::from("    ");
            if self.enable_ansi {
                s.push_str(&line.cyan().to_string());
            } else {
                s.push_str(line);
            }
            s
        } else {
            line.to_string()
        }
    }
}

