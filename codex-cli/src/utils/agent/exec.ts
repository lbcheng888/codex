import type { AppConfig } from "../config.js";
import type { SpawnOptions } from "child_process";
import type { ParseEntry } from "shell-quote";

import { process_patch } from "./apply-patch.js";
import { formatCommandForDisplay } from "../../format-command.js";
import { log } from "../logger/log.js";
import fs from "fs";
import path from "path";
import { parse } from "shell-quote";
import { resolvePathAgainstWorkdir } from "src/approvals.js";
import { PATCH_SUFFIX } from "src/parse-apply-patch.js";
import { spawn } from "child_process";

const DEFAULT_TIMEOUT_MS = 10_000; // 10 seconds

export type ExecInput = {
  cmd: Array<string>;
  workdir: string | undefined;
  timeoutInMillis: number | undefined;
};

export type ExecResult = {
  stdout: string;
  stderr: string;
  exitCode: number;
};

function requiresShell(cmd: Array<string>): boolean {
  // If the command is a single string that contains shell operators,
  // it needs to be run with shell: true
  if (cmd.length === 1 && cmd[0] !== undefined) {
    const tokens = parse(cmd[0]) as Array<ParseEntry>;
    return tokens.some((token) => typeof token === "object" && "op" in token);
  }

  // If the command is split into multiple arguments, we don't need shell: true
  // even if one of the arguments is a shell operator like '|'
  return false;
}

/**
 * Execute command with no restrictions or sandboxing
 */
export function exec(
  {
    cmd,
    workdir,
    timeoutInMillis,
  }: ExecInput,
  config: AppConfig,
  abortSignal?: AbortSignal,
): Promise<ExecResult> {
  return new Promise((resolve) => {
    const [command, ...args] = cmd;
    if (!command) {
      resolve({
        stdout: "",
        stderr: "No command provided",
        exitCode: 1,
      });
      return;
    }

    const options: SpawnOptions = {
      timeout: timeoutInMillis || DEFAULT_TIMEOUT_MS,
      ...(requiresShell(cmd) ? { shell: true } : {}),
      ...(workdir ? { cwd: workdir } : {}),
    };

    const child = spawn(command, args, options);
    
    let stdout = "";
    let stderr = "";

    child.stdout?.on("data", (data) => {
      stdout += data.toString();
    });

    child.stderr?.on("data", (data) => {
      stderr += data.toString();
    });

    child.on("close", (code) => {
      resolve({
        stdout,
        stderr,
        exitCode: code || 0,
      });
    });

    child.on("error", (error) => {
      resolve({
        stdout,
        stderr: error.message,
        exitCode: 1,
      });
    });

    // Some tests provide a jest.fn() shim or a minimal stub that does not
    // expose the full EventTarget interface.  Guard against this so that we
    // don't attempt to call `addEventListener` on objects that merely satisfy
    // the TypeScript structural type but lack the actual method at runtime.

    if (
      abortSignal &&
      typeof (abortSignal as unknown as { addEventListener?: unknown })
        .addEventListener === "function"
    ) {
      abortSignal.addEventListener("abort", () => {
        child.kill();
      });
    }
  });
}

export function execApplyPatch(
  patchText: string,
  workdir: string | undefined = undefined,
): ExecResult {
  // This find/replace is required from some models like 4.1 where the patch
  // text is wrapped in quotes that breaks the apply_patch command.
  let applyPatchInput = patchText
    .replace(/('|")?<<('|")EOF('|")/, "")
    .replace(/\*\*\* End Patch\nEOF('|")?/, "*** End Patch")
    .trim();

  if (!applyPatchInput.endsWith(PATCH_SUFFIX)) {
    applyPatchInput += "\n" + PATCH_SUFFIX;
  }

  log(`Applying patch: \`\`\`${applyPatchInput}\`\`\`\n\n`);

  try {
    const result = process_patch(
      applyPatchInput,
      (p) => fs.readFileSync(resolvePathAgainstWorkdir(p, workdir), "utf8"),
      (p, c) => {
        const resolvedPath = resolvePathAgainstWorkdir(p, workdir);

        // Ensure the parent directory exists before writing the file. This
        // mirrors the behaviour of the standalone apply_patch CLI (see
        // write_file() in apply-patch.ts) and prevents errors when adding a
        // new file in a not‑yet‑created sub‑directory.
        const dir = path.dirname(resolvedPath);
        if (dir !== ".") {
          fs.mkdirSync(dir, { recursive: true });
        }

        fs.writeFileSync(resolvedPath, c, "utf8");
      },
      (p) => fs.unlinkSync(resolvePathAgainstWorkdir(p, workdir)),
    );
    return {
      stdout: result,
      stderr: "",
      exitCode: 0,
    };
  } catch (error: unknown) {
    // @ts-expect-error error might not be an object or have a message property.
    const stderr = String(error.message ?? error);
    return {
      stdout: "",
      stderr: stderr,
      exitCode: 1,
    };
  }
}

export function getBaseCmd(cmd: Array<string>): string {
  const formattedCommand = formatCommandForDisplay(cmd);
  return formattedCommand.split(" ")[0] || cmd[0] || "<unknown>";
}
