use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;
use tokio::io::BufReader;
use tokio::process::Child;
use tokio::process::Command;
use tokio::sync::Notify;
use tracing::trace;

use crate::error::CodexErr;
use crate::error::Result;

// Constants for timeout and signal handling
const DEFAULT_TIMEOUT_MS: u64 = 30_000; // 30 seconds
const TIMEOUT_CODE: i32 = 124;
const SIGKILL_CODE: i32 = 9;

// Maximum output limits
const MAX_OUTPUT_BYTES: usize = 1024 * 1024; // 1MB
const MAX_OUTPUT_LINES: usize = 10_000;

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

    // Execute command directly without any sandboxing
    let raw_output_result = exec(params, ctrl_c).await;

    match raw_output_result {
        Ok(output) => {
            let elapsed = start.elapsed();
            Ok(ExecToolCallOutput {
                exit_code: output.exit_code(),
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

#[derive(Debug)]
pub(crate) struct RawExecToolCallOutput {
    exit_status: ExitStatus,
    stdout: String,
    stderr: String,
}

impl RawExecToolCallOutput {
    fn exit_code(&self) -> i32 {
        self.exit_status.code().unwrap_or(-1)
    }
}

async fn exec(
    ExecParams {
        command,
        cwd,
        timeout_ms,
        env,
    }: ExecParams,
    ctrl_c: Arc<Notify>,
) -> Result<RawExecToolCallOutput> {
    let (program, args) = command.split_first().ok_or_else(|| {
        CodexErr::Io(io::Error::new(
            io::ErrorKind::InvalidInput,
            "command args are empty",
        ))
    })?;
    let arg0 = None;
    let child = spawn_child_async(
        PathBuf::from(program),
        args.into(),
        arg0,
        cwd,
        StdioPolicy::RedirectForShellTool,
        env,
    )
    .await?;
    consume_truncated_output(child, ctrl_c, timeout_ms).await
}

#[derive(Debug, Clone, Copy)]
pub enum StdioPolicy {
    RedirectForShellTool,
    Inherit,
}

/// Spawns a child process for the ExecParams,
/// ensuring the args and environment variables used to create the `Command`
/// (and `Child`) honor the configuration.
async fn spawn_child_async(
    program: PathBuf,
    args: Vec<String>,
    #[cfg_attr(not(unix), allow(unused_variables))] arg0: Option<&str>,
    cwd: PathBuf,
    stdio_policy: StdioPolicy,
    env: HashMap<String, String>,
) -> std::io::Result<Child> {
    trace!("spawn_child_async: {program:?} {args:?} {arg0:?} {cwd:?} {stdio_policy:?} {env:?}");

    let mut cmd = Command::new(&program);
    #[cfg(unix)]
    cmd.arg0(arg0.map_or_else(|| program.to_string_lossy().to_string(), String::from));
    cmd.args(args);
    cmd.current_dir(cwd);
    cmd.env_clear();
    cmd.envs(env);

    // If this Codex process dies (including being killed via SIGKILL), we want
    // any child processes that were spawned as part of a `"shell"` tool call
    // to also be terminated.

    // This relies on prctl(2), so it only works on Linux.
    #[cfg(target_os = "linux")]
    unsafe {
        cmd.pre_exec(|| {
            // This prctl call effectively requests, "deliver SIGTERM when my
            // current parent dies."
            if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGTERM) == -1 {
                return Err(io::Error::last_os_error());
            }

            // Though if there was a race condition and this pre_exec() block is
            // run _after_ the parent (i.e., the Codex process) has already
            // exited, then the parent is the _init_ process (which will never
            // die), so we should just terminate the child process now.
            if libc::getppid() == 1 {
                libc::raise(libc::SIGTERM);
            }
            Ok(())
        });
    }

    match stdio_policy {
        StdioPolicy::RedirectForShellTool => {
            // Do not create a file descriptor for stdin because otherwise some
            // commands may hang forever waiting for input. For example, ripgrep has
            // a heuristic where it may try to read from stdin as explained here:
            // https://github.com/BurntSushi/ripgrep/blob/e2362d4d5185d02fa857bf381e7bd52e66fafc73/crates/core/flags/hiargs.rs#L1101-L1103
            cmd.stdin(Stdio::null());

            cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        }
        StdioPolicy::Inherit => {
            // Inherit stdin, stdout, and stderr from the parent process.
            cmd.stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit());
        }
    }

    cmd.kill_on_drop(true).spawn()
}

/// Consumes the output of a child process, truncating it so it is suitable for
/// use as the output of a `shell` tool call. Also enforces specified timeout.
pub(crate) async fn consume_truncated_output(
    mut child: Child,
    ctrl_c: Arc<Notify>,
    timeout_ms: Option<u64>,
) -> Result<RawExecToolCallOutput> {
    // Both stdout and stderr were configured with `Stdio::piped()`
    // above, therefore `take()` should normally return `Some`.  If it doesn't
    // we treat it as an exceptional I/O error

    let stdout_reader = child.stdout.take().ok_or_else(|| {
        CodexErr::Io(io::Error::other(
            "stdout pipe was unexpectedly not available",
        ))
    })?;
    let stderr_reader = child.stderr.take().ok_or_else(|| {
        CodexErr::Io(io::Error::other(
            "stderr pipe was unexpectedly not available",
        ))
    })?;

    let stdout_handle = tokio::spawn(read_capped(
        BufReader::new(stdout_reader),
        MAX_OUTPUT_BYTES,
        MAX_OUTPUT_LINES,
    ));
    let stderr_handle = tokio::spawn(read_capped(
        BufReader::new(stderr_reader),
        MAX_OUTPUT_BYTES,
        MAX_OUTPUT_LINES,
    ));

    let interrupted = ctrl_c.notified();
    let timeout = Duration::from_millis(timeout_ms.unwrap_or(DEFAULT_TIMEOUT_MS));
    let exit_status = tokio::select! {
        result = tokio::time::timeout(timeout, child.wait()) => {
            match result {
                Ok(Ok(exit_status)) => exit_status,
                Ok(e) => e?,
                Err(_) => {
                    // timeout
                    child.start_kill()?;
                    // Debatable whether `child.wait().await` should be called here.
                    synthetic_exit_status(128 + TIMEOUT_CODE)
                }
            }
        }
        _ = interrupted => {
            child.start_kill()?;
            synthetic_exit_status(128 + SIGKILL_CODE)
        }
    };

    let stdout = stdout_handle.await??;
    let stderr = stderr_handle.await??;

    Ok(RawExecToolCallOutput {
        exit_status,
        stdout: String::from_utf8_lossy(&stdout).to_string(),
        stderr: String::from_utf8_lossy(&stderr).to_string(),
    })
}

async fn read_capped<R: AsyncRead + Unpin>(
    mut reader: R,
    max_output: usize,
    max_lines: usize,
) -> io::Result<Vec<u8>> {
    let mut buf = Vec::with_capacity(max_output.min(8 * 1024));
    let mut tmp = [0u8; 8192];

    let mut remaining_bytes = max_output;
    let mut remaining_lines = max_lines;

    loop {
        let n = reader.read(&mut tmp).await?;
        if n == 0 {
            break;
        }

        // Copy into the buffer only while we still have byte and line budget.
        if remaining_bytes > 0 && remaining_lines > 0 {
            let mut copy_len = 0;
            for &b in &tmp[..n] {
                if remaining_bytes == 0 || remaining_lines == 0 {
                    break;
                }
                copy_len += 1;
                remaining_bytes -= 1;
                if b == b'\n' {
                    remaining_lines -= 1;
                }
            }
            buf.extend_from_slice(&tmp[..copy_len]);
        }
        // Continue reading to EOF to avoid back-pressure, but discard once caps are hit.
    }

    Ok(buf)
}

#[cfg(unix)]
fn synthetic_exit_status(code: i32) -> ExitStatus {
    use std::os::unix::process::ExitStatusExt;
    std::process::ExitStatus::from_raw(code)
}

#[cfg(windows)]
fn synthetic_exit_status(code: i32) -> ExitStatus {
    use std::os::windows::process::ExitStatusExt;
    #[expect(clippy::unwrap_used)]
    std::process::ExitStatus::from_raw(code.try_into().unwrap())
}
