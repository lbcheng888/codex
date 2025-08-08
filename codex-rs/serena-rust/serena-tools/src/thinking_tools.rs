use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::Result;
use tracing::debug;

/// Tool for thinking about collected information
pub struct ThinkAboutCollectedInformation;

#[async_trait::async_trait]
impl Tool for ThinkAboutCollectedInformation {
    fn name(&self) -> &'static str {
        "think_about_collected_information"
    }

    fn description(&self) -> &'static str {
        "Analyze and reflect on information collected during the current session to determine next steps"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "context": {
                    "type": "string",
                    "description": "Current context or task being worked on"
                },
                "information_summary": {
                    "type": "string", 
                    "description": "Summary of information collected so far"
                }
            }
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        let context = input["context"].as_str().unwrap_or("general");
        let info_summary = input["information_summary"].as_str().unwrap_or("");

        debug!(target = "thinking", context = %context, "thinking about collected information");

        // This is a meta-cognitive tool that helps the AI reflect on its progress
        let analysis = json!({
            "reflection": "This tool helps analyze the information gathering process and determine if sufficient context has been collected.",
            "context": context,
            "information_summary": info_summary,
            "recommendations": [
                "Review the collected information for completeness",
                "Identify any gaps in understanding",
                "Determine if additional information gathering is needed",
                "Plan next steps based on current knowledge"
            ],
            "timestamp": time::OffsetDateTime::now_utc().to_string()
        });

        Ok(analysis)
    }
}

/// Tool for thinking about task adherence
pub struct ThinkAboutTaskAdherence;

#[async_trait::async_trait]
impl Tool for ThinkAboutTaskAdherence {
    fn name(&self) -> &'static str {
        "think_about_task_adherence"
    }

    fn description(&self) -> &'static str {
        "Reflect on whether current actions align with the original task and objectives"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "original_task": {
                    "type": "string",
                    "description": "The original task or objective"
                },
                "current_actions": {
                    "type": "string",
                    "description": "Summary of current actions being taken"
                }
            }
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        let original_task = input["original_task"].as_str().unwrap_or("not specified");
        let current_actions = input["current_actions"].as_str().unwrap_or("not specified");

        debug!(target = "thinking", original_task = %original_task, "thinking about task adherence");

        let analysis = json!({
            "reflection": "This tool helps ensure that current work remains aligned with the original objectives.",
            "original_task": original_task,
            "current_actions": current_actions,
            "adherence_check": [
                "Are current actions directly contributing to the original task?",
                "Have we deviated from the intended scope?",
                "Are we solving the right problem?",
                "Should we refocus or continue current approach?"
            ],
            "recommendations": [
                "Regularly check alignment with original objectives",
                "Avoid scope creep unless explicitly requested",
                "Ask for clarification if task requirements are unclear",
                "Document any necessary scope changes"
            ],
            "timestamp": time::OffsetDateTime::now_utc().to_string()
        });

        Ok(analysis)
    }
}

/// Tool for thinking about whether work is complete
pub struct ThinkAboutWhetherYouAreDone;

#[async_trait::async_trait]
impl Tool for ThinkAboutWhetherYouAreDone {
    fn name(&self) -> &'static str {
        "think_about_whether_you_are_done"
    }

    fn description(&self) -> &'static str {
        "Evaluate whether the current task has been completed satisfactorily"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "task_description": {
                    "type": "string",
                    "description": "Description of the task being evaluated"
                },
                "work_completed": {
                    "type": "string",
                    "description": "Summary of work completed so far"
                },
                "success_criteria": {
                    "type": "string",
                    "description": "Criteria for considering the task complete"
                }
            }
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        let task_desc = input["task_description"].as_str().unwrap_or("not specified");
        let work_completed = input["work_completed"].as_str().unwrap_or("not specified");
        let success_criteria = input["success_criteria"].as_str().unwrap_or("not specified");

        debug!(target = "thinking", task = %task_desc, "evaluating task completion");

        let analysis = json!({
            "reflection": "This tool helps determine if the current task has been completed to satisfaction.",
            "task_description": task_desc,
            "work_completed": work_completed,
            "success_criteria": success_criteria,
            "completion_checklist": [
                "Have all requirements been addressed?",
                "Is the solution working as expected?",
                "Are there any edge cases not handled?",
                "Is documentation/testing adequate?",
                "Would the user be satisfied with this result?"
            ],
            "next_steps": [
                "If complete: Summarize results and close task",
                "If incomplete: Identify remaining work",
                "If uncertain: Ask for user feedback",
                "If blocked: Identify and communicate blockers"
            ],
            "timestamp": time::OffsetDateTime::now_utc().to_string()
        });

        Ok(analysis)
    }
}

/// Tool for summarizing changes made
pub struct SummarizeChanges;

#[async_trait::async_trait]
impl Tool for SummarizeChanges {
    fn name(&self) -> &'static str {
        "summarize_changes"
    }

    fn description(&self) -> &'static str {
        "Summarize all changes made during the current session for review and documentation"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "session_context": {
                    "type": "string",
                    "description": "Context or purpose of the current session"
                },
                "files_modified": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of files that were modified"
                },
                "changes_description": {
                    "type": "string",
                    "description": "Description of changes made"
                }
            }
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        let session_context = input["session_context"].as_str().unwrap_or("general work");
        let files_modified = input["files_modified"].as_array()
            .map(|arr| arr.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>())
            .unwrap_or_default();
        let changes_desc = input["changes_description"].as_str().unwrap_or("not specified");

        debug!(target = "thinking", files_count = files_modified.len(), "summarizing changes");

        let summary = json!({
            "session_summary": {
                "context": session_context,
                "files_modified": files_modified,
                "file_count": files_modified.len(),
                "changes_description": changes_desc,
                "timestamp": time::OffsetDateTime::now_utc().to_string()
            },
            "recommendations": [
                "Review all changes for correctness",
                "Test modified functionality",
                "Update documentation if needed",
                "Consider creating a backup or commit",
                "Verify no unintended side effects"
            ],
            "next_actions": [
                "Run tests to verify changes",
                "Update related documentation",
                "Commit changes with descriptive message",
                "Notify relevant stakeholders if needed"
            ]
        });

        Ok(summary)
    }
}
