/**
 * Minimal shared types for representing sandbox-related structures.  These
 * were extracted so that higher-level agent code can compile and run
 * regardless of whether a full sandbox implementation is available for the
 * current platform.  Only the fields required by the existing codebase are
 * included; additional properties can be added later as needed.
 */

// ---------------------------------------------------------------------------
// Sandbox type discriminator
// ---------------------------------------------------------------------------

export enum SandboxType {
  /**
   * The command will be run directly on the host without any sandboxing or
   * additional restrictions.
   */
  NONE = "none",

  /**
   * The command is executed inside a dedicated sandbox (e.g. macOS seatbelt
   * or a container).  The runtime pick between different sandbox providers
   * depending on the platform and user configuration.
   */
  SANDBOXED = "sandboxed",
}

// ---------------------------------------------------------------------------
// ExecInput mirrors the structure expected by exec()/handleExecCommand().  A
// subset is used elsewhere (e.g. parseToolCallArguments()) so we purposefully
// keep all fields optional except `cmd`.
// ---------------------------------------------------------------------------

export type ExecInput = {
  /**
   * The argv-style command to run, e.g. `["git", "status"]`.
   */
  cmd: Array<string>;

  /**
   * Optional working directory that should be used as the process cwd.
   */
  workdir?: string;

  /**
   * Optional timeout (in milliseconds) after which the process will be
   * forcibly terminated.
   */
  timeoutInMillis?: number;
};

// ---------------------------------------------------------------------------
// Metadata returned alongside stdout/stderr so that the caller can display
// execution details (exit code, duration, …) without having to parse the raw
// output.
// ---------------------------------------------------------------------------

export type ExecOutputMetadata = {
  exit_code: number;
  duration_seconds: number;
  // Allow any additional implementation-specific metadata without having to
  // update the type downstream.
  [key: string]: unknown;
};

