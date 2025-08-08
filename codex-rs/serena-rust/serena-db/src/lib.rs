//! Database repository layer (SQLite + PostgreSQL).
//!
//! This crate currently provides a reference implementation for a simple
//! `UserRepo` using SQLx.  It is totally optional and not yet wired into
//! the higher-level application.  It serves as an initial scaffold for the
//! Phase-2 "I/O 层适配" 里程碑。

use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use sqlx::{AnyPool, Row};

/// Public user model shared by repos.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    pub id: i64,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewUser {
    pub name: String,
    pub email: String,
}

/// Repository trait – allows swapping backend (SQLite / Postgres) and
/// enables dual-write in future.
#[async_trait]
pub trait UserRepo: Send + Sync + 'static {
    async fn create(&self, u: NewUser) -> Result<User>;
    async fn find(&self, id: i64) -> Result<Option<User>>;
}

/// Implementation backed by `sqlx::AnyPool`.  Works with both SQLite and
/// Postgres depending on `DATABASE_URL`.
pub struct AnyUserRepo {
    pool: AnyPool,
}

impl AnyUserRepo {
    pub fn new(pool: AnyPool) -> Self { Self { pool } }

    /// Convenience helper to connect to the database url and install all drivers
    /// when the `any` feature is used.
    pub async fn connect(database_url: &str) -> Result<Self> {
        sqlx::any::install_default_drivers();
        let pool = AnyPool::connect(database_url).await?;
        Ok(Self { pool })
    }
}

#[async_trait]
impl UserRepo for AnyUserRepo {
    async fn create(&self, u: NewUser) -> Result<User> {
        // Use positional bind (works for both backends with SQLx Any)
        let row = sqlx::query("INSERT INTO users (name, email) VALUES (?1, ?2) RETURNING id, name, email")
            .bind(&u.name)
            .bind(&u.email)
            .fetch_one(&self.pool)
            .await?;

        Ok(User { id: row.get::<i64, _>(0), name: row.get::<String, _>(1), email: row.get::<String, _>(2) })
    }

    async fn find(&self, id: i64) -> Result<Option<User>> {
        let maybe = sqlx::query("SELECT id, name, email FROM users WHERE id = ?1")
            .bind(id)
            .fetch_optional(&self.pool)
            .await?;

        Ok(maybe.map(|row| User { id: row.get(0), name: row.get(1), email: row.get(2) }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MEMORY_DB: &str = "sqlite::memory:";

    #[tokio::test]
    async fn crud_in_memory_sqlite() -> Result<()> {
        sqlx::any::install_default_drivers();
        let pool = sqlx::any::AnyPoolOptions::new()
            .max_connections(1)
            .connect(MEMORY_DB)
            .await?;
        // Create table
        sqlx::query("CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, email TEXT)")
            .execute(&pool)
            .await?;

        let repo = AnyUserRepo::new(pool);
        let alice = repo
            .create(NewUser { name: "Alice".into(), email: "alice@example.com".into() })
            .await?;
        assert_eq!(alice.id, 1);

        let fetched = repo.find(alice.id).await?.expect("found");
        assert_eq!(fetched, alice);
        Ok(())
    }
}
