import type {
  ExecInput,
  ExecOutputMetadata,
} from "./agent/sandbox/interface.js";
import type { ResponseFunctionToolCall } from "openai/resources/responses/responses.mjs";

import { log } from "node:console";
import { formatCommandForDisplay } from "src/format-command.js";

// The console utility import is intentionally explicit to avoid bundlers from
// including the entire `console` module when only the `log` function is
// required.

export function parseToolCallOutput(toolCallOutput: string): {
  output: string;
  metadata: ExecOutputMetadata;
} {
  try {
    const { output, metadata } = JSON.parse(toolCallOutput);
    return {
      output,
      metadata,
    };
  } catch (err) {
    return {
      output: `Failed to parse JSON result`,
      metadata: {
        exit_code: 1,
        duration_seconds: 0,
      },
    };
  }
}

export type CommandReviewDetails = {
  cmd: Array<string>;
  cmdReadableText: string;
  workdir: string | undefined;
};

/**
 * Tries to parse a tool call and, if successful, returns an object that has
 * both:
 * - an array of strings to use with `ExecInput` and `canAutoApprove()`
 * - a human-readable string to display to the user
 */
export function parseToolCall(
  toolCall: ResponseFunctionToolCall,
): CommandReviewDetails | undefined {
  const toolCallArgs = parseToolCallArguments(toolCall.arguments);
  if (toolCallArgs == null) {
    return undefined;
  }

  const { cmd, workdir } = toolCallArgs;
  const cmdReadableText = formatCommandForDisplay(cmd);

  return {
    cmd,
    cmdReadableText,
    workdir,
  };
}

/**
 * If toolCallArguments is a string of JSON that can be parsed into an object
 * with a "cmd" or "command" property that is an `Array<string>`, then returns
 * that array. Otherwise, returns undefined.
 */
export function parseToolCallArguments(
  toolCallArguments: string,
): ExecInput | undefined {
  let json: unknown;
  try {
    json = JSON.parse(toolCallArguments);
  } catch (err) {
    log(`Failed to parse toolCall.arguments: ${toolCallArguments}`);
    return undefined;
  }

  if (typeof json !== "object" || json == null) {
    return undefined;
  }

  // Some models (e.g. xAI Grok, Claude function-calling) wrap the actual
  // arguments in an extra { arguments: { … } } envelope that also carries a
  // top-level `tool_name` / `name` field.  Detect and unwrap this scenario so
  // that we can reuse the same extraction logic regardless of the exact
  // nesting produced by the model.

  // If we see an `arguments` property *and* it's an object, treat its content
  // as the real payload to look for `cmd` / `command` etc.  This keeps the
  // original behaviour fully intact for the standard shape while expanding
  // support to the nested variant.

  if (
    "arguments" in (json as Record<string, unknown>) &&
    typeof (json as Record<string, unknown>).arguments === "object" &&
    (json as Record<string, unknown>).arguments !== null
  ) {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    json = (json as Record<string, unknown>).arguments as Record<string, unknown>;
  }

  const { cmd, command } = json as Record<string, unknown>;
  // The OpenAI model sometimes produces a single string instead of an array.
  // Accept both shapes:
  const commandArray =
    toStringArray(cmd) ??
    toStringArray(command) ??
    (typeof cmd === "string" ? [cmd] : undefined) ??
    (typeof command === "string" ? [command] : undefined);
  if (commandArray == null) {
    return undefined;
  }

  // @ts-expect-error timeout and workdir may not exist on json.
  const { timeout, workdir } = json;
  return {
    cmd: commandArray,
    workdir: typeof workdir === "string" ? workdir : undefined,
    timeoutInMillis: typeof timeout === "number" ? timeout : undefined,
  };
}

function toStringArray(obj: unknown): Array<string> | undefined {
  if (Array.isArray(obj) && obj.every((item) => typeof item === "string")) {
    const arrayOfStrings: Array<string> = obj;
    return arrayOfStrings;
  } else {
    return undefined;
  }
}
