use owo_colors::OwoColorize;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag, TagEnd};
use once_cell::sync::Lazy;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style, Theme};
use syntect::parsing::{SyntaxReference, SyntaxSet};
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};

/// Render a subset of Markdown to a single ANSI‑styled String suitable for terminal output.
/// When `enable_ansi` is false, styling is stripped and plain text is returned.
pub fn render_markdown_to_ansi(src: &str, enable_ansi: bool) -> String {
    // Configure pulldown with common options; we don't need tables/footnotes extensions for now.
    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_STRIKETHROUGH);
    let parser = Parser::new_ext(src, opts);

    // Rendering state
    let mut out = String::new();
    let mut bold = false;
    let mut italic = false;
    let mut in_code_block = false;
    let mut code_block_lang: Option<String> = None;
    let mut code_block_buf = String::new();
    let mut pending_list_indent = 0usize; // nesting level
    let mut in_list_item = false;
    let mut link_url: Option<String> = None;
    let mut link_text: String = String::new();

    for ev in parser {
        match ev {
            Event::Start(tag) => match tag {
                Tag::Heading { .. } => {
                    bold = true;
                }
                Tag::Emphasis => {
                    italic = true;
                }
                Tag::Strong => {
                    bold = true;
                }
                Tag::CodeBlock(kind) => {
                    in_code_block = true;
                    code_block_buf.clear();
                    if let CodeBlockKind::Fenced(lang) = kind {
                        code_block_lang = Some(lang.to_string());
                    } else {
                        code_block_lang = None;
                    }
                }
                Tag::List(_start) => {
                    pending_list_indent += 1;
                }
                Tag::Item => {
                    in_list_item = true;
                    // Start a new list line with appropriate indentation and bullet
                    if pending_list_indent > 0 {
                        out.push_str(&"  ".repeat(pending_list_indent - 1));
                    }
                    out.push_str("- ");
                }
                Tag::Link { dest_url, .. } => {
                    link_url = Some(dest_url.to_string());
                    link_text.clear();
                }
                _ => {}
            },
            Event::End(tag_end) => match tag_end {
                TagEnd::Heading(_) => {
                    bold = false;
                    out.push_str("\n\n");
                }
                TagEnd::Emphasis => {
                    italic = false;
                }
                TagEnd::Strong => {
                    bold = false;
                }
                TagEnd::CodeBlock => {
                    in_code_block = false;
                    // Render fenced code block with indentation and color/highlighting
                    let content = code_block_buf.trim_end_matches('\n');
                    out.push('\n');
                    if enable_ansi {
                        out.push_str(&highlight_block_with_lang(content, code_block_lang.as_deref()));
                    } else {
                        for line in content.lines() {
                            out.push_str("    ");
                            out.push_str(line);
                            out.push('\n');
                        }
                    }
                    out.push('\n');
                    code_block_buf.clear();
                    code_block_lang = None;
                }
                TagEnd::List(_start) => {
                    if pending_list_indent > 0 {
                        pending_list_indent -= 1;
                    }
                    out.push('\n');
                }
                TagEnd::Item => {
                    in_list_item = false;
                    out.push('\n');
                }
                TagEnd::Link { .. } => {
                    let url = link_url.take().unwrap_or_default();
                    if url.is_empty() {
                        out.push_str(&link_text);
                    } else {
                        // Render as "text (url)"
                        out.push_str(&link_text);
                        out.push(' ');
                        out.push('(');
                        out.push_str(&url);
                        out.push(')');
                    }
                    link_text.clear();
                }
                _ => {}
            },
            Event::Text(cow) => {
                let s: &str = &cow;
                if in_code_block {
                    code_block_buf.push_str(s);
                } else if link_url.is_some() {
                    link_text.push_str(s);
                } else {
                    out.push_str(&apply_inline_styles(s, bold, italic, enable_ansi));
                }
            }
            Event::Code(code) => {
                let t: &str = &code;
                let rendered = if enable_ansi { format!("`{}`", t).cyan().to_string() } else { format!("`{}`", t) };
                out.push_str(&rendered);
            }
            Event::SoftBreak | Event::HardBreak => {
                if in_code_block {
                    code_block_buf.push('\n');
                } else {
                    out.push('\n');
                    if in_list_item && pending_list_indent > 0 {
                        out.push_str(&"  ".repeat(pending_list_indent - 1));
                        out.push_str("  ");
                    }
                }
            }
            Event::Rule => {
                // Horizontal rule: render as a dim line
                let line = "―".repeat(20);
                let rendered = if enable_ansi { line.dimmed().to_string() } else { line };
                out.push_str(&rendered);
                out.push_str("\n\n");
            }
            Event::Html(cow) | Event::InlineHtml(cow) => {
                // Keep raw HTML as is
                out.push_str(&cow);
            }
            _ => {}
        }
    }

    out.trim_end_matches('\n').to_string()
}

fn apply_inline_styles(s: &str, bold: bool, italic: bool, enable_ansi: bool) -> String {
    if !enable_ansi {
        return s.to_string();
    }
    match (bold, italic) {
        (true, true) => s.bold().italic().to_string(),
        (true, false) => s.bold().to_string(),
        (false, true) => s.italic().to_string(),
        (false, false) => s.to_string(),
    }
}

// Global syntax/theme sets for highlighting
static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(|| SyntaxSet::load_defaults_newlines());
static THEME: Lazy<Theme> = Lazy::new(|| {
    let ts = syntect::highlighting::ThemeSet::load_defaults();
    // Choose a widely available theme
    ts.themes
        .get("base16-ocean.dark")
        .cloned()
        .unwrap_or_else(|| ts.themes.values().next().cloned().unwrap())
});

fn find_syntax_for_lang<'a>(lang: Option<&str>) -> Option<&'a SyntaxReference> {
    let ss: &'a SyntaxSet = unsafe { &*(&*SYNTAX_SET as *const SyntaxSet) };
    if let Some(lang) = lang {
        // Try token (name) first, then extension
        ss.find_syntax_by_token(lang)
            .or_else(|| ss.find_syntax_by_extension(lang))
    } else {
        None
    }
}

fn highlight_block_with_lang(src: &str, lang: Option<&str>) -> String {
    let ss: &SyntaxSet = &SYNTAX_SET;
    let theme: &Theme = &THEME;
    let syntax = find_syntax_for_lang::<'_>(lang).unwrap_or_else(|| ss.find_syntax_plain_text());
    let mut h = HighlightLines::new(syntax, theme);
    let mut out = String::new();
    for line in LinesWithEndings::from(src) {
        let ranges: Vec<(Style, &str)> = h.highlight_line(line, ss).unwrap_or_default();
        let ansi = as_24_bit_terminal_escaped(&ranges[..], false);
        out.push_str("    ");
        out.push_str(&ansi);
    }
    out
}
