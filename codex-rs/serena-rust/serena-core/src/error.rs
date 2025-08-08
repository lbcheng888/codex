use thiserror::Error;
use serde::{Deserialize, Serialize};

/// Core error types for the Serena system
#[derive(Debug, Error, Clone, Serialize, Deserialize)]
pub enum CoreError {
    #[error("Configuration error: {message}")]
    Config {
        message: String,
    },

    #[error("I/O error: {message}")]
    Io {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        path: Option<String>,
    },

    #[error("Protocol error: {message}")]
    Protocol {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        code: Option<i32>,
    },

    #[error("Tool execution error: {message}")]
    Tool {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        tool_name: Option<String>,
    },

    #[error("Validation error: {message}")]
    Validation {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        field: Option<String>,
    },

    #[error("Resource not found: {message}")]
    NotFound {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        resource_type: Option<String>,
    },

    #[error("Language server error: {message}")]
    LanguageServer {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        language: Option<String>,
    },

    #[error("Timeout error: {message}")]
    Timeout {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        duration_ms: Option<u64>,
    },

    #[error("Serialization error: {message}")]
    Serialization {
        message: String,
    },

    #[error("Authentication error: {message}")]
    Authentication {
        message: String,
    },

    #[error("Permission denied: {message}")]
    Permission {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        required_permission: Option<String>,
    },

    #[error("Internal error: {message}")]
    Internal {
        message: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        context: Option<String>,
    },
}

pub type Result<T> = std::result::Result<T, CoreError>;

impl CoreError {
    /// Create a configuration error
    pub fn config<S: Into<String>>(message: S) -> Self {
        Self::Config {
            message: message.into()
        }
    }

    /// Create an I/O error
    pub fn io<S: Into<String>>(message: S) -> Self {
        Self::Io {
            message: message.into(),
            path: None
        }
    }

    /// Create an I/O error with path
    pub fn io_with_path<S: Into<String>, P: Into<String>>(message: S, path: P) -> Self {
        Self::Io {
            message: message.into(),
            path: Some(path.into())
        }
    }

    /// Create a validation error
    pub fn validation<S: Into<String>>(message: S) -> Self {
        Self::Validation {
            message: message.into(),
            field: None
        }
    }

    /// Create a validation error with field
    pub fn validation_with_field<S: Into<String>, F: Into<String>>(message: S, field: F) -> Self {
        Self::Validation {
            message: message.into(),
            field: Some(field.into())
        }
    }

    /// Create a tool error
    pub fn tool<S: Into<String>>(message: S) -> Self {
        Self::Tool {
            message: message.into(),
            tool_name: None
        }
    }

    /// Create a tool error with tool name
    pub fn tool_with_name<S: Into<String>, T: Into<String>>(message: S, tool_name: T) -> Self {
        Self::Tool {
            message: message.into(),
            tool_name: Some(tool_name.into())
        }
    }

    /// Create a not found error
    pub fn not_found<S: Into<String>>(message: S) -> Self {
        Self::NotFound {
            message: message.into(),
            resource_type: None
        }
    }

    /// Create a timeout error
    pub fn timeout<S: Into<String>>(message: S, duration_ms: u64) -> Self {
        Self::Timeout {
            message: message.into(),
            duration_ms: Some(duration_ms)
        }
    }

    /// Create an internal error
    pub fn internal<S: Into<String>>(message: S) -> Self {
        Self::Internal {
            message: message.into(),
            context: None
        }
    }

    /// Create a permission denied error
    pub fn permission_denied<S: Into<String>>(message: S) -> Self {
        Self::Permission {
            message: message.into(),
            required_permission: None,
        }
    }

    /// Check if this error is recoverable
    pub fn is_recoverable(&self) -> bool {
        match self {
            CoreError::Timeout { .. } => true,
            CoreError::Protocol { .. } => true,
            CoreError::LanguageServer { .. } => true,
            CoreError::Io { .. } => true,
            _ => false,
        }
    }

    /// Get error category for metrics/logging
    pub fn category(&self) -> &'static str {
        match self {
            CoreError::Config { .. } => "config",
            CoreError::Io { .. } => "io",
            CoreError::Protocol { .. } => "protocol",
            CoreError::Tool { .. } => "tool",
            CoreError::Validation { .. } => "validation",
            CoreError::NotFound { .. } => "not_found",
            CoreError::LanguageServer { .. } => "language_server",
            CoreError::Timeout { .. } => "timeout",
            CoreError::Serialization { .. } => "serialization",
            CoreError::Authentication { .. } => "authentication",
            CoreError::Permission { .. } => "permission",
            CoreError::Internal { .. } => "internal",
        }
    }
}

// Standard library error conversions
impl From<std::io::Error> for CoreError {
    fn from(e: std::io::Error) -> Self {
        CoreError::io(e.to_string())
    }
}

impl From<serde_json::Error> for CoreError {
    fn from(e: serde_json::Error) -> Self {
        CoreError::Serialization {
            message: e.to_string()
        }
    }
}

impl From<serde_yaml::Error> for CoreError {
    fn from(e: serde_yaml::Error) -> Self {
        CoreError::Serialization {
            message: e.to_string()
        }
    }
}

impl From<tokio::time::error::Elapsed> for CoreError {
    fn from(e: tokio::time::error::Elapsed) -> Self {
        CoreError::Timeout {
            message: e.to_string(),
            duration_ms: None,
        }
    }
}