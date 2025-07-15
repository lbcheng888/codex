use anyhow::Result;
use qdrant_client::qdrant::{
    SearchPointsBuilder, Filter, FieldCondition, Match, Range,
    PayloadIncludeSelector, PayloadExcludeSelector,
};

use crate::types::{CodeContext, Language};

/// Builder for complex vector search queries
pub struct QueryBuilder {
    collection_name: String,
    vector: Vec<f32>,
    limit: u64,
    filter: Option<Filter>,
    payload_selector: Option<PayloadIncludeSelector>,
    score_threshold: Option<f32>,
}

impl QueryBuilder {
    pub fn new(collection_name: String, vector: Vec<f32>, limit: u64) -> Self {
        Self {
            collection_name,
            vector,
            limit,
            filter: None,
            payload_selector: None,
            score_threshold: None,
        }
    }

    pub fn with_filter(mut self, filter: Filter) -> Self {
        self.filter = Some(filter);
        self
    }

    pub fn with_language_filter(mut self, language: Language) -> Self {
        let language_filter = Filter::must([
            FieldCondition::new(
                "language",
                Match::new(format!("{:?}", language)),
            ).into(),
        ]);

        self.filter = match self.filter {
            Some(existing_filter) => Some(Filter::must([
                existing_filter.into(),
                language_filter.into(),
            ])),
            None => Some(language_filter),
        };

        self
    }

    pub fn with_path_filter(mut self, path_pattern: String) -> Self {
        let path_filter = Filter::must([
            FieldCondition::new(
                "file_path",
                Match::new(path_pattern),
            ).into(),
        ]);

        self.filter = match self.filter {
            Some(existing_filter) => Some(Filter::must([
                existing_filter.into(),
                path_filter.into(),
            ])),
            None => Some(path_filter),
        };

        self
    }

    pub fn with_score_threshold(mut self, threshold: f32) -> Self {
        self.score_threshold = Some(threshold);
        self
    }

    pub fn with_payload_selector(mut self, fields: Vec<String>) -> Self {
        self.payload_selector = Some(PayloadIncludeSelector::new(fields));
        self
    }

    pub fn build(self) -> SearchPointsBuilder {
        let mut builder = SearchPointsBuilder::new(
            self.collection_name,
            self.vector,
            self.limit,
        );

        if let Some(filter) = self.filter {
            builder = builder.with_filter(filter);
        }

        if let Some(selector) = self.payload_selector {
            builder = builder.with_payload(selector);
        } else {
            builder = builder.with_payload(true);
        }

        if let Some(threshold) = self.score_threshold {
            builder = builder.score_threshold(threshold);
        }

        builder
    }
}

impl Default for QueryBuilder {
    fn default() -> Self {
        Self::new("default".to_string(), vec![0.0; 768], 10)
    }
}