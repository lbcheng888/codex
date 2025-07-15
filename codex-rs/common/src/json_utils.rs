//! Safe JSON serialization and deserialization utilities
//! 
//! This module provides safe alternatives to unwrap() calls when working with JSON.

#[cfg(feature = "json-utils")]
use anyhow::{Context, Result};
#[cfg(feature = "json-utils")]
use serde::Serialize;
#[cfg(feature = "json-utils")]
use serde_json;

/// Safely serialize a value to JSON string with proper error handling
#[cfg(feature = "json-utils")]
pub fn to_json_string<T: Serialize>(value: &T) -> Result<String> {
    serde_json::to_string(value).context("Failed to serialize value to JSON")
}

/// Safely serialize a value to pretty-printed JSON string
#[cfg(feature = "json-utils")]
pub fn to_json_string_pretty<T: Serialize>(value: &T) -> Result<String> {
    serde_json::to_string_pretty(value).context("Failed to serialize value to pretty JSON")
}

/// Safely parse JSON from string with proper error handling
#[cfg(feature = "json-utils")]
pub fn from_json_str<'a, T: serde::Deserialize<'a>>(s: &'a str) -> Result<T> {
    serde_json::from_str(s).context("Failed to parse JSON from string")
}

/// Safely parse JSON from bytes with proper error handling
#[cfg(feature = "json-utils")]
pub fn from_json_slice<'a, T: serde::Deserialize<'a>>(v: &'a [u8]) -> Result<T> {
    serde_json::from_slice(v).context("Failed to parse JSON from bytes")
}

#[cfg(test)]
#[cfg(feature = "json-utils")]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct TestStruct {
        name: String,
        value: i32,
    }

    #[test]
    fn test_json_roundtrip() {
        let original = TestStruct {
            name: "test".to_string(),
            value: 42,
        };

        let json_str = to_json_string(&original).expect("Serialization should succeed");
        let parsed: TestStruct = from_json_str(&json_str).expect("Deserialization should succeed");

        assert_eq!(original, parsed);
    }

    #[test]
    fn test_pretty_json() {
        let test_struct = TestStruct {
            name: "test".to_string(),
            value: 42,
        };

        let pretty_json = to_json_string_pretty(&test_struct).expect("Pretty serialization should succeed");
        assert!(pretty_json.contains('\n')); // Pretty JSON should contain newlines
    }

    #[test]
    fn test_invalid_json_error() {
        let result: Result<TestStruct> = from_json_str("invalid json");
        assert!(result.is_err());
    }
}