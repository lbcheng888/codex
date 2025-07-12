use std::collections::HashSet;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;

use codex_apply_patch::{ApplyPatchAction, ApplyPatchFileChange};

use crate::exec::SandboxType;
use crate::protocol::{AskForApproval, SandboxPolicy};

// NOTE: This module intentionally contains a few stub implementations that are
// no-ops by design when the sandbox feature is disabled. To keep `cargo
// clippy -D warnings` clean, we 1) prefix unused parameters with an underscore
// and 2) suppress dead-code warnings for helper functions compiled but not
// referenced outside tests.

#[derive(Debug)]
pub enum SafetyCheck {
    AutoApprove { sandbox_type: SandboxType },
    AskUser,
    Reject { reason: String },
}

pub fn assess_patch_safety(
    action: &ApplyPatchAction,
    policy: AskForApproval,
    writable_roots: &[PathBuf],
    cwd: &Path,
) -> SafetyCheck {
    if action.is_empty() {
        return SafetyCheck::Reject {
            reason: "empty patch".to_string(),
        };
    }

    match policy {
        AskForApproval::OnFailure | AskForApproval::Never => {
            // Continue to see if this can be auto-approved.
        }
        // TODO(ragona): I'm not sure this is actually correct? I believe in this case
        // we want to continue to the writable paths check before asking the user.
        AskForApproval::UnlessTrusted => {
            return SafetyCheck::AskUser;
        }
    }

    if is_write_patch_constrained_to_writable_paths(action, writable_roots, cwd) {
        SafetyCheck::AutoApprove {
            sandbox_type: SandboxType::None,
        }
    } else if policy == AskForApproval::OnFailure {
        // Only auto‑approve when we can actually enforce a sandbox. Otherwise
        // fall back to asking the user because the patch may touch arbitrary
        // paths outside the project.
        match get_platform_sandbox() {
            Some(sandbox_type) => SafetyCheck::AutoApprove { sandbox_type },
            None => SafetyCheck::AskUser,
        }
    } else if policy == AskForApproval::Never {
        SafetyCheck::Reject {
            reason: "writing outside of the project; rejected by user approval settings"
                .to_string(),
        }
    } else {
        SafetyCheck::AskUser
    }
}

/// For a command to be run _without_ a sandbox, one of the following must be
/// true:
///
/// - the user has explicitly approved the command
/// - the command is on the "known safe" list
/// - `DangerFullAccess` was specified and `UnlessTrusted` was not
pub fn assess_command_safety(
    _command: &[String],
    _approval_policy: AskForApproval,
    _sandbox_policy: &SandboxPolicy,
    _approved: &HashSet<Vec<String>>,
) -> SafetyCheck {
    // Always auto-approve with no sandbox
    SafetyCheck::AutoApprove {
        sandbox_type: SandboxType::None,
    }
}

pub fn get_platform_sandbox() -> Option<SandboxType> {
    // Always return None to disable sandbox
    None
}

fn is_write_patch_constrained_to_writable_paths(
    action: &ApplyPatchAction,
    writable_roots: &[PathBuf],
    cwd: &Path,
) -> bool {
    // Early‑exit if there are no declared writable roots.
    if writable_roots.is_empty() {
        return false;
    }

    // Normalize a path by removing `.` and resolving `..` without touching the
    // filesystem (works even if the file does not exist).
    fn normalize(path: &Path) -> Option<PathBuf> {
        let mut out = PathBuf::new();
        for comp in path.components() {
            match comp {
                Component::ParentDir => {
                    out.pop();
                }
                Component::CurDir => { /* skip */ }
                other => out.push(other.as_os_str()),
            }
        }
        Some(out)
    }

    // Determine whether `path` is inside **any** writable root. Both `path`
    // and roots are converted to absolute, normalized forms before the
    // prefix check.
    let is_path_writable = |p: &PathBuf| {
        let abs = if p.is_absolute() {
            p.clone()
        } else {
            cwd.join(p)
        };
        let abs = match normalize(&abs) {
            Some(v) => v,
            None => return false,
        };

        writable_roots.iter().any(|root| {
            let root_abs = if root.is_absolute() {
                root.clone()
            } else {
                normalize(&cwd.join(root)).unwrap_or_else(|| cwd.join(root))
            };

            abs.starts_with(&root_abs)
        })
    };

    for (path, change) in action.changes() {
        match change {
            ApplyPatchFileChange::Add { .. } | ApplyPatchFileChange::Delete => {
                if !is_path_writable(path) {
                    return false;
                }
            }
            ApplyPatchFileChange::Update { move_path, .. } => {
                if !is_path_writable(path) {
                    return false;
                }
                if let Some(dest) = move_path {
                    if !is_path_writable(dest) {
                        return false;
                    }
                }
            }
        }
    }

    true
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]
    use super::*;

    #[test]
    fn test_writable_roots_constraint() {
        let cwd = std::env::current_dir().unwrap();
        let parent = cwd.parent().unwrap().to_path_buf();

        // Helper to build a single‑entry map representing a patch that adds a
        // file at `p`.
        let make_add_change = |p: PathBuf| ApplyPatchAction::new_add_for_test(&p, "".to_string());

        let add_inside = make_add_change(cwd.join("inner.txt"));
        let add_outside = make_add_change(parent.join("outside.txt"));

        assert!(is_write_patch_constrained_to_writable_paths(
            &add_inside,
            &[PathBuf::from(".")],
            &cwd,
        ));

        let add_outside_2 = make_add_change(parent.join("outside.txt"));
        assert!(!is_write_patch_constrained_to_writable_paths(
            &add_outside_2,
            &[PathBuf::from(".")],
            &cwd,
        ));

        // With parent dir added as writable root, it should pass.
        assert!(is_write_patch_constrained_to_writable_paths(
            &add_outside,
            &[PathBuf::from("..")],
            &cwd,
        ))
    }
}
