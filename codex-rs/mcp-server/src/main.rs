use codex_mcp_server::run_main;

fn main() -> anyhow::Result<()> {
    tokio::runtime::Runtime::new()?.block_on(async {
        run_main(None).await?;
        Ok(())
    })
}
