#!/usr/bin/env python3
"""
Test script to verify Grok function calling works via direct API call.
This simulates what TUI mode should be doing.
"""

import json
import os
import openai

def test_grok_function_calling():
    print("Testing Grok function calling directly...")
    
    # Set up the client
    client = openai.OpenAI(
        api_key=os.environ["XAI_API_KEY"],
        base_url="https://api.x.ai/v1"
    )
    
    # Test with a simple ls command request
    tools = [
        {
            "type": "function",
            "function": {
                "name": "bash",
                "description": "Execute shell command",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "command": {
                            "type": "string",
                            "description": "Shell command to execute"
                        }
                    },
                    "required": ["command"]
                }
            }
        }
    ]
    
    messages = [
        {"role": "system", "content": "You are a helpful assistant that can execute shell commands."},
        {"role": "user", "content": "Please run the ls command to list files in the current directory."}
    ]
    
    print("Sending request to Grok...")
    print(f"Messages: {json.dumps(messages, indent=2)}")
    print(f"Tools: {json.dumps(tools, indent=2)}")
    
    try:
        response = client.chat.completions.create(
            model="grok-4-0709",
            messages=messages,
            tools=tools,
            stream=True  # Test streaming mode like TUI
        )
        
        print("\nStreaming response:")
        function_call_found = False
        
        for chunk in response:
            print(f"Chunk: {chunk}")
            
            if chunk.choices and len(chunk.choices) > 0:
                choice = chunk.choices[0]
                
                # Check for tool calls
                if hasattr(choice.delta, 'tool_calls') and choice.delta.tool_calls:
                    function_call_found = True
                    print(f"FOUND TOOL CALL: {choice.delta.tool_calls}")
                
                # Check for finish reason
                if choice.finish_reason:
                    print(f"Finish reason: {choice.finish_reason}")
        
        print(f"\nFunction call found: {function_call_found}")
        return function_call_found
        
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    success = test_grok_function_calling()
    print(f"\nTest {'PASSED' if success else 'FAILED'}")