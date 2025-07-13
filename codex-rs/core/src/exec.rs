#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Instant;

use tokio::io::AsyncReadExt;
use tokio::io::BufReader;
use tokio::process::Child;
use tokio::process::Command;
use tokio::sync::Notify;

use crate::error::CodexErr;
use crate::error::Result;

#[derive(Debug, Clone)]
pub struct ExecParams {
    pub command: Vec<String>,
    pub cwd: PathBuf,
    pub timeout_ms: Option<u64>,
    pub env: HashMap<String, String>,
}

pub async fn process_exec_tool_call(
    params: ExecParams,
    ctrl_c: Arc<Notify>,
) -> Result<ExecToolCallOutput> {
    let start = Instant::now();
    let raw_output_result = exec(params, ctrl_c).await;

    match raw_output_result {
        Ok(output) => {
            let elapsed = start.elapsed();
            Ok(ExecToolCallOutput {
                exit_code: output.exit_code,
                duration_seconds: elapsed.as_secs_f64(),
                stdout: output.stdout,
                stderr: output.stderr,
            })
        }
        Err(err) => Err(err),
    }
}

#[derive(Debug)]
pub struct ExecToolCallOutput {
    pub exit_code: i32,
    pub duration_seconds: f64,
    pub stdout: String,
    pub stderr: String,
}

async fn exec(
    params: ExecParams,
    _ctrl_c: Arc<Notify>,
) -> Result<ExecToolCallOutput> {
    let mut child = spawn_child(&params)?;

    let child_stdout = child.stdout.take();
    let child_stderr = child.stderr.take();

    let mut stdout_output = String::new();
    let mut stderr_output = String::new();

    if let Some(stdout) = child_stdout {
        let mut reader = BufReader::new(stdout);
        reader.read_to_string(&mut stdout_output).await.ok();
    }

    if let Some(stderr) = child_stderr {
        let mut reader = BufReader::new(stderr);
        reader.read_to_string(&mut stderr_output).await.ok();
    }

    let exit_status = child.wait().await?;
    let exit_code = get_exit_code(exit_status);

    Ok(ExecToolCallOutput {
        exit_code,
        duration_seconds: 0.0,
        stdout: stdout_output,
        stderr: stderr_output,
    })
}

fn spawn_child(params: &ExecParams) -> Result<Child> {
    let command_name = &params.command[0];
    let args = &params.command[1..];

    let mut cmd = Command::new(command_name);
    cmd.args(args)
        .current_dir(&params.cwd)
        .envs(&params.env)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .stdin(Stdio::null());

    cmd.spawn().map_err(|e| CodexErr::Io(e))
}

fn get_exit_code(exit_status: ExitStatus) -> i32 {
    if let Some(code) = exit_status.code() {
        code
    } else {
        #[cfg(unix)]
        {
            if let Some(signal) = exit_status.signal() {
                128 + signal
            } else {
                1
            }
        }
        #[cfg(not(unix))]
        {
            1
        }
    }
}