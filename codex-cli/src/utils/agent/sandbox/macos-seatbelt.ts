/**
 * This file provides a *stub* implementation of the macOS seatbelt sandbox
 * support that Codex uses when running shell commands.  The full seatbelt
 * binary is only required when sandboxing is enabled which is not the case in
 * most CI/test environments.  Exporting the handful of symbols that the rest
 * of the codebase expects is therefore sufficient to unblock the runtime
 * without pulling in the heavy native dependency.
 */

// Path to the seatbelt executable.  In production this points to the actual
// compiled binary that performs the sandboxing.  For our testing purposes we
// default to an empty string which signals the caller that sandbox execution
// is unavailable on the current platform.

export const PATH_TO_SEATBELT_EXECUTABLE = "";

// The real implementation contains helper functions for spawning commands
// inside the sandbox.  We intentionally leave these out to keep the stub small
// – any attempt to use them would fail anyway because the seatbelt binary is
// missing.

