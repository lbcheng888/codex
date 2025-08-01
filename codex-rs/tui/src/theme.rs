//! Theme system for unified colors and styles across the TUI
//! Inspired by Claude Code's visual design

use ratatui::prelude::*;

/// Main theme structure containing all UI colors and styles
#[derive(Clone)]
pub struct Theme {
    /// Primary colors for key UI elements
    pub primary: ThemePrimary,
    /// Colors for different message types  
    pub messages: ThemeMessages,
    /// UI element colors
    pub ui: ThemeUI,
    /// Status and state colors
    pub status: ThemeStatus,
}

#[derive(Clone)]
pub struct ThemePrimary {
    /// Primary brand color - used for focused elements
    #[allow(dead_code)]
    pub accent: Color,
    /// Secondary accent color  
    #[allow(dead_code)]
    pub accent_secondary: Color,
    /// Background color for the main area
    #[allow(dead_code)]
    pub background: Color,
    /// Foreground/text color
    pub foreground: Color,
}

#[derive(Clone)]
pub struct ThemeMessages {
    /// User message colors
    pub user: MessageColors,
    /// Assistant/AI message colors  
    pub assistant: MessageColors,
    /// System message colors
    #[allow(dead_code)]
    pub system: MessageColors,
    /// Error message colors
    #[allow(dead_code)]
    pub error: MessageColors,
}

#[derive(Clone)]
pub struct MessageColors {
    /// Background color for message bubbles (now unused, kept for compatibility)
    pub background: Color,
    /// Text color
    pub foreground: Color,
    /// Border color (now unused, kept for compatibility)
    pub border: Color,
}

#[derive(Clone)]
pub struct ThemeUI {
    /// Border colors for different states
    pub border_focused: Color,
    pub border_unfocused: Color,
    /// Input field colors
    #[allow(dead_code)]
    pub input_background: Color,
    #[allow(dead_code)]
    pub input_foreground: Color,
    /// Selection/highlight colors
    pub selection: Color,
    pub highlight: Color,
}

#[derive(Clone)]
pub struct ThemeStatus {
    /// Success state color
    pub success: Color,
    /// Warning state color  
    pub warning: Color,
    /// Error state color
    pub error: Color,
    /// Info state color
    pub info: Color,
    /// Loading/progress color
    pub loading: Color,
}

impl Default for Theme {
    fn default() -> Self {
        Self::claude_inspired()
    }
}

impl Theme {
    /// Claude Code inspired theme with bright, high-contrast colors
    pub fn claude_inspired() -> Self {
        Self::claude_inspired_with_contrast(false)
    }

    /// High contrast version of Claude Code theme for accessibility
    #[allow(dead_code)]
    pub fn high_contrast() -> Self {
        Self::claude_inspired_with_contrast(true)
    }

    /// Claude Code theme with optional high contrast mode
    pub fn claude_inspired_with_contrast(high_contrast: bool) -> Self {
        let (bg_main, fg_main) = if high_contrast {
            (Color::Rgb(0, 0, 0), Color::Rgb(255, 255, 255)) // Pure black bg, pure white fg
        } else {
            // Use the terminal's canonical `Color::White` constant instead of an
            // RGB approximation so that terminals limited to 8/16 colours map
            // it to bright-white (colour 15) instead of a dull light-grey.  A
            // custom `Rgb(250,250,250)` often degrades to colour 7 (grey) in
            // such terminals, which explains the “history panel looks grey”
            // issue that users have reported.
            (Color::Rgb(13, 13, 13), Color::White)
        };

        Self {
            primary: ThemePrimary {
                accent: Color::Rgb(247, 144, 61), // Claude's signature orange
                accent_secondary: Color::Rgb(79, 172, 254), // Claude's blue accent
                background: bg_main,
                foreground: fg_main,
            },
            messages: ThemeMessages {
                user: MessageColors {
                    background: if high_contrast {
                        Color::Rgb(25, 25, 25)
                    } else {
                        Color::Rgb(28, 28, 28)
                    },
                    foreground: Color::White, // bright white for maximum contrast
                    border: Color::Rgb(79, 172, 254), // Claude blue for user messages
                },
                assistant: MessageColors {
                    background: if high_contrast {
                        Color::Rgb(18, 18, 18)
                    } else {
                        Color::Rgb(22, 22, 22)
                    },
                    foreground: Color::White,
                    border: Color::Rgb(247, 144, 61), // Claude orange for assistant messages
                },
                system: MessageColors {
                    background: Color::Rgb(45, 55, 72),    // Blue-gray
                    foreground: Color::Rgb(255, 255, 255), // Pure white for maximum readability
                    border: Color::Rgb(180, 180, 180),     // Light border
                },
                error: MessageColors {
                    background: Color::Rgb(127, 29, 29),   // Dark red
                    foreground: Color::Rgb(255, 200, 200), // Bright light red
                    border: Color::Rgb(255, 100, 100),     // Bright red border
                },
            },
            ui: ThemeUI {
                border_focused: Color::Rgb(247, 144, 61), // Claude orange when focused
                border_unfocused: Color::Rgb(60, 62, 73), // Subtle gray when unfocused
                input_background: Color::Rgb(25, 26, 35), // Darker input background
                input_foreground: Color::Rgb(245, 245, 245), // Softer white input text
                selection: Color::Rgb(79, 172, 254),      // Claude blue selection
                highlight: Color::Rgb(247, 144, 61),      // Claude orange highlight
            },
            status: ThemeStatus {
                success: Color::Rgb(100, 255, 150), // Bright green
                warning: Color::Rgb(255, 220, 100), // Bright yellow
                error: Color::Rgb(255, 120, 120),   // Bright red
                info: Color::Rgb(120, 180, 255),    // Bright blue
                loading: Color::Rgb(200, 150, 255), // Bright purple
            },
        }
    }

    /// Helper method to create border style for focused state (now unused)
    #[allow(dead_code)]
    pub fn focused_border_style(&self) -> Style {
        Style::default()
            .fg(self.ui.border_focused)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method to create border style for unfocused state (now unused)
    #[allow(dead_code)]
    pub fn unfocused_border_style(&self) -> Style {
        Style::default().fg(self.ui.border_unfocused)
    }

    /// Helper method for user message style (now unused)
    #[allow(dead_code)]
    pub fn user_message_style(&self) -> Style {
        Style::default()
            .fg(self.messages.user.foreground)
            .bg(self.messages.user.background)
    }

    /// Helper method for assistant message style (now unused)
    #[allow(dead_code)]
    pub fn assistant_message_style(&self) -> Style {
        Style::default()
            .fg(self.messages.assistant.foreground)
            .bg(self.messages.assistant.background)
    }

    /// Helper method for command/code style
    pub fn code_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(235, 235, 235)) // Slightly softer white for code
            .bg(Color::Rgb(20, 21, 28)) // Even darker background for code blocks
    }

    /// Helper method for emphasized/highlighted text
    pub fn emphasis_style(&self) -> Style {
        Style::default()
            .fg(self.ui.highlight)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for dimmed/secondary text - subtle gray for hierarchy
    pub fn dim_style(&self) -> Style {
        Style::default().fg(Color::Rgb(180, 180, 180)) // Subtle gray for secondary text
    }

    /// Helper method for popup/modal background
    pub fn popup_background_style(&self) -> Style {
        Style::default()
            .bg(Color::Rgb(40, 44, 52)) // Dark popup background
            .fg(self.primary.foreground)
    }

    /// Helper method for popup/modal border
    pub fn popup_border_style(&self) -> Style {
        Style::default()
            .fg(self.ui.highlight)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for input field style
    #[allow(dead_code)]
    pub fn input_style(&self) -> Style {
        Style::default()
            .fg(self.ui.input_foreground)
            .bg(self.ui.input_background)
    }

    /// Helper method for active/selected item style
    pub fn active_item_style(&self) -> Style {
        Style::default()
            .fg(self.ui.selection) // Use selection color for text instead of background
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
    }

    /// Helper method for inactive/unselected item style
    pub fn inactive_item_style(&self) -> Style {
        Style::default().fg(Color::Rgb(230, 230, 230)) // Bright white for primary content
    }

    /// Helper method for subtle metadata/labels
    #[allow(dead_code)]
    pub fn metadata_style(&self) -> Style {
        Style::default().fg(Color::Rgb(160, 160, 160)) // Subtle gray for metadata
    }

    /// Helper method for message headers with proper hierarchy
    pub fn message_header_style(&self, is_user: bool) -> Style {
        if is_user {
            Style::default()
                .fg(self.primary.accent_secondary)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default()
                .fg(self.primary.accent)
                .add_modifier(Modifier::BOLD)
        }
    }

    /// Helper method for keyboard shortcut hint style
    #[allow(dead_code)]
    pub fn shortcut_hint_style(&self) -> Style {
        Style::default()
            .fg(self.ui.highlight)
            .add_modifier(Modifier::ITALIC)
    }

    /// Helper method for loading/progress indicator style
    pub fn loading_style(&self) -> Style {
        Style::default()
            .fg(self.status.loading)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for separator line between panes
    pub fn separator_style(&self) -> Style {
        Style::default().fg(Color::Rgb(60, 62, 73)) // Even more subtle separator to match Claude
    }

    /// Helper method for focus indicator style
    #[allow(dead_code)]
    pub fn focus_indicator_style(&self) -> Style {
        Style::default()
            .fg(self.ui.highlight)
            .bg(Color::Rgb(40, 44, 52)) // Dark background for indicator
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for keyboard shortcut key style
    #[allow(dead_code)]
    pub fn shortcut_key_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(26, 27, 38)) // Dark text
            .bg(self.ui.highlight) // Bright background
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for help text style
    pub fn help_text_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(255, 255, 255)) // Pure white for maximum readability
            .add_modifier(Modifier::ITALIC)
    }

    /// Helper method for help overlay background
    #[allow(dead_code)]
    pub fn help_overlay_background(&self) -> Style {
        Style::default()
            .bg(Color::Rgb(30, 35, 45)) // Dark overlay background
            .fg(self.primary.foreground)
    }

    /// Helper method for help section header style
    #[allow(dead_code)]
    pub fn help_section_header_style(&self) -> Style {
        Style::default()
            .fg(self.ui.highlight)
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
    }

    /// Helper method for help category style
    #[allow(dead_code)]
    pub fn help_category_style(&self) -> Style {
        Style::default()
            .fg(self.status.info)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for modern user message bubble style
    #[allow(dead_code)]
    pub fn user_bubble_style(&self) -> Style {
        Style::default()
            .fg(self.messages.user.foreground)
            .bg(Color::Rgb(45, 55, 72)) // Slightly lighter than background
    }

    /// Helper method for modern assistant message bubble style  
    #[allow(dead_code)]
    pub fn assistant_bubble_style(&self) -> Style {
        Style::default()
            .fg(self.messages.assistant.foreground)
            .bg(Color::Rgb(35, 45, 60)) // Subtle blue-gray background
    }

    /// Helper method for message timestamp style
    #[allow(dead_code)]
    pub fn message_timestamp_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(120, 120, 120)) // Dim gray
            .add_modifier(Modifier::ITALIC)
    }

    /// Helper method for message bubble border style (now unused)
    #[allow(dead_code)]
    pub fn bubble_border_style(&self, is_user: bool) -> Style {
        if is_user {
            Style::default().fg(self.messages.user.border)
        } else {
            Style::default().fg(self.messages.assistant.border)
        }
    }

    /// Helper method for rounded bubble corner characters (now unused)
    #[allow(dead_code)]
    pub fn bubble_corners(&self) -> (&'static str, &'static str, &'static str, &'static str) {
        // Top-left, Top-right, Bottom-left, Bottom-right
        ("╭", "╮", "╰", "╯")
    }

    /// Helper method for message content padding style (now unused)
    #[allow(dead_code)]
    pub fn message_content_style(&self, is_user: bool) -> Style {
        if is_user {
            self.user_message_style()
        } else {
            self.assistant_message_style()
        }
    }

    /// Helper method for smooth transition highlighting
    #[allow(dead_code)]
    pub fn transition_highlight_style(&self) -> Style {
        Style::default()
            .fg(self.primary.foreground)
            .bg(Color::Rgb(60, 90, 120)) // Blended highlight color
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for success indication
    pub fn success_indicator_style(&self) -> Style {
        Style::default()
            .fg(self.status.success)
            .add_modifier(Modifier::BOLD)
    }

    /// Helper method for warning indication
    #[allow(dead_code)]
    pub fn warning_indicator_style(&self) -> Style {
        Style::default()
            .fg(self.status.warning)
            .add_modifier(Modifier::BOLD)
    }

    /// Check if this theme provides high contrast for accessibility
    #[allow(dead_code)]
    pub fn is_high_contrast(&self) -> bool {
        // Simple heuristic: check if background is pure black
        self.primary.background == Color::Rgb(0, 0, 0)
    }

    /// Get accessibility-friendly style for important notifications
    #[allow(dead_code)]
    pub fn accessibility_notification_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(255, 255, 255)) // Pure white
            .bg(Color::Rgb(0, 100, 200)) // High contrast blue
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
    }

    /// Get style for focus indicators that meets accessibility standards
    #[allow(dead_code)]
    pub fn accessible_focus_style(&self) -> Style {
        Style::default()
            .fg(Color::Rgb(0, 0, 0)) // Black text
            .bg(Color::Rgb(255, 255, 0)) // Bright yellow background
            .add_modifier(Modifier::BOLD)
    }
}
