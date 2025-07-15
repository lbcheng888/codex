//! Session management for Codex conversations
//!
//! This module provides functionality to save, load, and manage conversation sessions
//! as suggested in the improvement roadmap.

use std::path::{Path, PathBuf};
use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::models::ResponseItem;

/// Session metadata containing information about a saved conversation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionMetadata {
    /// Unique identifier for the session
    pub id: String,
    /// Human-readable title for the session
    pub title: String,
    /// When the session was created
    pub created_at: DateTime<Utc>,
    /// When the session was last modified
    pub last_modified: DateTime<Utc>,
    /// Number of messages in the session
    pub message_count: usize,
    /// Brief summary of the conversation
    pub summary: Option<String>,
}

/// Session data including the full conversation history
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionData {
    /// Session metadata
    pub metadata: SessionMetadata,
    /// The complete conversation history as a list of response items
    pub conversation: Vec<ResponseItem>,
}

/// Manager for saving and loading conversation sessions
pub struct SessionManager {
    /// Base directory for storing sessions (typically ~/.codex/sessions)
    sessions_dir: PathBuf,
}

impl SessionManager {
    /// Create a new session manager with the specified sessions directory
    pub fn new(codex_home: &Path) -> Self {
        let sessions_dir = codex_home.join("sessions");
        Self { sessions_dir }
    }

    /// Ensure the sessions directory exists
    pub async fn initialize(&self) -> Result<()> {
        if !self.sessions_dir.exists() {
            tokio::fs::create_dir_all(&self.sessions_dir).await
                .context("Failed to create sessions directory")?;
        }
        Ok(())
    }

    /// Generate a unique session ID based on timestamp
    pub fn generate_session_id() -> String {
        let now = Utc::now();
        format!("{}_{}", now.format("%Y%m%d_%H%M%S"), now.timestamp_subsec_micros())
    }

    /// Save a conversation session to disk
    pub async fn save_session(
        &self,
        session_id: &str,
        title: &str,
        conversation: &[ResponseItem],
        summary: Option<String>,
    ) -> Result<()> {
        self.initialize().await?;

        let now = Utc::now();
        let metadata = SessionMetadata {
            id: session_id.to_string(),
            title: title.to_string(),
            created_at: now,
            last_modified: now,
            message_count: conversation.len(),
            summary,
        };

        let session_data = SessionData {
            metadata,
            conversation: conversation.to_vec(),
        };

        let session_file = self.get_session_file_path(session_id);
        let json_content = serde_json::to_string_pretty(&session_data)
            .context("Failed to serialize session data")?;

        tokio::fs::write(&session_file, &json_content).await
            .context("Failed to write session file")?;

        Ok(())
    }

    /// Load a conversation session from disk
    pub async fn load_session(&self, session_id: &str) -> Result<SessionData> {
        let session_file = self.get_session_file_path(session_id);
        
        let json_content = tokio::fs::read_to_string(&session_file).await
            .context("Failed to read session file")?;

        let session_data: SessionData = serde_json::from_str(&json_content)
            .context("Failed to deserialize session data")?;

        Ok(session_data)
    }

    /// List all available sessions
    pub async fn list_sessions(&self) -> Result<Vec<SessionMetadata>> {
        if !self.sessions_dir.exists() {
            return Ok(Vec::new());
        }

        let mut sessions = Vec::new();
        let mut dir_entries = tokio::fs::read_dir(&self.sessions_dir).await
            .context("Failed to read sessions directory")?;

        while let Some(entry) = dir_entries.next_entry().await
            .context("Failed to read directory entry")? {
            
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Some(file_stem) = path.file_stem().and_then(|s| s.to_str()) {
                    match self.load_session(file_stem).await {
                        Ok(session_data) => sessions.push(session_data.metadata),
                        Err(e) => {
                            tracing::warn!("Failed to load session {}: {}", file_stem, e);
                        }
                    }
                }
            }
        }

        // Sort by last modified time, newest first
        sessions.sort_by(|a, b| b.last_modified.cmp(&a.last_modified));
        Ok(sessions)
    }

    /// Get the most recent session ID, if any
    pub async fn get_last_session_id(&self) -> Result<Option<String>> {
        let sessions = self.list_sessions().await?;
        Ok(sessions.first().map(|s| s.id.clone()))
    }

    /// Delete a session
    pub async fn delete_session(&self, session_id: &str) -> Result<()> {
        let session_file = self.get_session_file_path(session_id);
        if session_file.exists() {
            tokio::fs::remove_file(&session_file).await
                .context("Failed to delete session file")?;
        }
        Ok(())
    }

    /// Get the file path for a session
    fn get_session_file_path(&self, session_id: &str) -> PathBuf {
        self.sessions_dir.join(format!("{}.json", session_id))
    }

    /// Create a session with auto-generated title based on content
    pub async fn create_auto_session(
        &self,
        conversation: &[ResponseItem],
    ) -> Result<String> {
        let session_id = Self::generate_session_id();
        
        // Generate a title from the first user message or use a default
        let title = self.generate_title_from_conversation(conversation);
        
        self.save_session(&session_id, &title, conversation, None).await?;
        Ok(session_id)
    }

    /// Generate a title from conversation content
    fn generate_title_from_conversation(&self, conversation: &[ResponseItem]) -> String {
        // Try to find the first message and create a title from it
        for item in conversation.iter() {
            // Extract text content from the first response item
            // This is simplified - in practice you'd want to handle different content types
            if let Ok(json) = serde_json::to_string(item) {
                if json.len() > 10 {
                    // Take the first 50 characters as title
                    let title: String = json.chars().take(50).collect();
                    return if title.len() < json.len() {
                        format!("{}...", title.trim())
                    } else {
                        title.trim().to_string()
                    };
                }
            }
        }
        
        // Fallback to timestamp-based title
        format!("Session {}", Utc::now().format("%Y-%m-%d %H:%M"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_session_manager_creation() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let codex_home = temp_dir.path();
        
        let session_manager = SessionManager::new(codex_home);
        assert_eq!(session_manager.sessions_dir, codex_home.join("sessions"));
    }

    #[tokio::test]
    async fn test_session_id_generation() {
        let id1 = SessionManager::generate_session_id();
        // Sleep briefly to ensure different microsecond timestamps
        tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
        let id2 = SessionManager::generate_session_id();
        
        assert_ne!(id1, id2);
        assert!(id1.len() > 0);
        assert!(id2.len() > 0);
        // Check that IDs follow the expected format (should have two underscores now)
        assert_eq!(id1.matches('_').count(), 2);
        assert_eq!(id2.matches('_').count(), 2);
    }

    #[test]
    fn test_session_metadata_serialization() {
        let metadata = SessionMetadata {
            id: "test_session".to_string(),
            title: "Test Session".to_string(),
            created_at: Utc::now(),
            last_modified: Utc::now(),
            message_count: 5,
            summary: Some("Test summary".to_string()),
        };

        let json = serde_json::to_string(&metadata).expect("Should serialize");
        let deserialized: SessionMetadata = serde_json::from_str(&json).expect("Should deserialize");
        
        assert_eq!(metadata.id, deserialized.id);
        assert_eq!(metadata.title, deserialized.title);
        assert_eq!(metadata.message_count, deserialized.message_count);
    }
}