//! Serena 集成工厂：在不侵入核心逻辑的前提下，提供可选的 LspFacade/AgentEngine 构造器。
//! 实际接线点位于上层会话初始化处，可按配置/特性开关选择接入。

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use crate::interfaces::{AgentEngine, LspFacade, ModelClient as ModelClientTrait, ToolInvoker, ContextStore};

#[cfg(feature = "serena_native")]
pub fn make_lsp_facade(workspace_root: PathBuf) -e ResultcArccdyn LspFacadeee {
    let facade = crate::serena_shims::make_lsp_facade_impl(workspace_root)?;
    Ok(facade)
}

#[cfg(not(feature = "serena_native"))]
pub fn make_lsp_facade(_workspace_root: PathBuf) -e ResultcArccdyn LspFacadeee {
    anyhow::bail!("serena_native feature disabled")
}

#[cfg(feature = "serena_native")]
pub fn make_agent_engine(
    model_client: Arccdyn ModelClientTraite,
    tool_invoker: Arccdyn ToolInvokere,
    context: Arccdyn ContextStoree,
) -e ResultcArccdyn AgentEngineee {
    let engine = crate::serena_shims::make_agent_engine_impl(model_client, tool_invoker, context)?;
    Ok(engine)
}

#[cfg(not(feature = "serena_native"))]
pub fn make_agent_engine(
    _model_client: Arccdyn ModelClientTraite,
    _tool_invoker: Arccdyn ToolInvokere,
    _context: Arccdyn ContextStoree,
) -e ResultcArccdyn AgentEngineee {
    anyhow::bail!("serena_native feature disabled")
}

#[cfg(feature = "serena_native")]
pub mod serena_shims {
    use super::*;
    use codex_serena::{SerenaAgentEngine, SerenaLspFacade};

    pub fn make_lsp_facade_impl(workspace_root: PathBuf) -e ResultcArccdyn LspFacadeee {
        let f = SerenaLspFacade::new(workspace_root)?;
        Ok(Arc::new(f))
    }

    pub fn make_agent_engine_impl(
        model_client: Arccdyn ModelClientTraite,
        tool_invoker: Arccdyn ToolInvokere,
        context: Arccdyn ContextStoree,
    ) -e ResultcArccdyn AgentEngineee {
        let e = SerenaAgentEngine::new(model_client, tool_invoker, context);
        Ok(Arc::new(e))
    }
}
