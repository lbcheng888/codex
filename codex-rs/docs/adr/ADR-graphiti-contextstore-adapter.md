# ADR: Introduce Graphiti-backed ContextStore adapter (feature-gated)

Status: Proposed
Date: 2025-08-08
Authors: codex + serena integration team

Context
- We are progressively integrating Serena native components into codex-rs with clean interfaces (ToolInvoker, LspFacade, AgentEngine, ModelClient, ContextStore).
- We also host graphiti-rust in this repository, providing graph storage (Cozo), retrieval (tantivy + vectors), and LLM clients.
- We want a pluggable ContextStore backed by Graphiti/Cozo to support durable session state, file snapshots, and future RAG integrations, without disrupting current builds.

Decision
- Add a new workspace crate integrations/codex-graphiti exporting a ContextStoreGraphiti adapter.
- Gate Graphiti dependencies behind a feature flag graphiti_context (off by default) to avoid build overhead or breaking changes.
- Initial Phase A implementation wires up a Graph/Cozo handle and ships a compiling scaffold that implements the ContextStore trait with default methods.
- Subsequent phases will implement save/load and snapshot/index APIs with concrete graph transactions and indices, and add migration tooling.

Alternatives considered
- Implement ContextStoreGraphiti inside codex-core directly: rejected to keep single-direction dependency and optionality.
- Use a separate binary process via MCP: rejected; native in-process is preferred for latency and simplicity.

Consequences
- Safe, opt-in path to adopt Graphiti-backed context with zero default impact.
- Clear location for future work to implement persistence, search, and RAG.

Rollout plan
- A: Land adapter crate and ADR (this change). Ensure builds unchanged by default.
- B: Implement KV save/load using Cozo tables; add config keys under [graphiti] for DB path and namespace. Done. Default enabled in core. File snapshots implemented. Env overrides via GRAPHITI_DB_PATH/GRAPHITI_NAMESPACE.
- C: Add file snapshot storage and retrieval; wire to editing pipeline when enabled.
- D: Add search indices and retrieval hooks for agentic planning.
- E: Stabilize, add tests and benchmarks, and consider making it the default.

Backout
- Disable the graphiti_context feature flag or remove the crate from workspace members.
