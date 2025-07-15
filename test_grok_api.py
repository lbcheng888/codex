#!/usr/bin/env python3
"""
Test script to verify Grok API function calling format
"""
import os
import json
import requests
from typing import Dict, Any

def test_grok_function_call():
    """Test Grok's function calling API directly"""
    
    # Get API key from environment
    api_key = os.environ.get('XAI_API_KEY')
    if not api_key:
        print("Error: XAI_API_KEY environment variable not set")
        return
    
    # API endpoint
    url = "https://api.x.ai/v1/chat/completions"
    
    # Headers
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }
    
    # Define a simple shell tool
    tools = [
        {
            "type": "function",
            "function": {
                "name": "shell",
                "description": "Execute a shell command",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "command": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Command and arguments as array"
                        }
                    },
                    "required": ["command"]
                }
            }
        }
    ]
    
    # Request payload
    payload = {
        "model": "grok-4-0709",
        "messages": [
            {
                "role": "system",
                "content": "You are a helpful assistant that can execute shell commands."
            },
            {
                "role": "user",
                "content": "Please list the files in the current directory using ls command"
            }
        ],
        "tools": tools,
        "tool_choice": "auto",
        "stream": False  # Non-streaming for easier debugging
    }
    
    print("Sending request to Grok API...")
    print(f"URL: {url}")
    print(f"Payload: {json.dumps(payload, indent=2)}")
    
    try:
        response = requests.post(url, headers=headers, json=payload)
        print(f"\nResponse status: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            print("\nResponse JSON:")
            print(json.dumps(result, indent=2))
            
            # Check for tool calls
            if 'choices' in result and len(result['choices']) > 0:
                choice = result['choices'][0]
                if 'message' in choice:
                    message = choice['message']
                    if 'tool_calls' in message:
                        print("\nTool calls found:")
                        for tool_call in message['tool_calls']:
                            print(f"- Function: {tool_call.get('function', {}).get('name')}")
                            print(f"  Arguments: {tool_call.get('function', {}).get('arguments')}")
                            print(f"  ID: {tool_call.get('id')}")
                    else:
                        print("\nNo tool calls in response")
                        if 'content' in message:
                            print(f"Assistant message: {message['content']}")
        else:
            print(f"Error: {response.text}")
            
    except Exception as e:
        print(f"Exception occurred: {e}")

def test_grok_streaming():
    """Test Grok's streaming API with function calling"""
    
    api_key = os.environ.get('XAI_API_KEY')
    if not api_key:
        print("Error: XAI_API_KEY environment variable not set")
        return
    
    url = "https://api.x.ai/v1/chat/completions"
    
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
        "Accept": "text/event-stream"
    }
    
    tools = [
        {
            "type": "function",
            "function": {
                "name": "shell",
                "description": "Execute a shell command",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "command": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Command and arguments as array"
                        }
                    },
                    "required": ["command"]
                }
            }
        }
    ]
    
    payload = {
        "model": "grok-4-0709",
        "messages": [
            {
                "role": "system",
                "content": "You are a helpful assistant that can execute shell commands."
            },
            {
                "role": "user",
                "content": "Execute ls command"
            }
        ],
        "tools": tools,
        "stream": True
    }
    
    print("\nTesting streaming API...")
    
    try:
        response = requests.post(url, headers=headers, json=payload, stream=True)
        print(f"Response status: {response.status_code}")
        
        if response.status_code == 200:
            print("\nStreaming response:")
            for line in response.iter_lines():
                if line:
                    line_str = line.decode('utf-8')
                    print(f"SSE: {line_str}")
                    
                    if line_str.startswith("data: "):
                        data = line_str[6:]  # Remove "data: " prefix
                        if data.strip() == "[DONE]":
                            print("Stream completed")
                            break
                        
                        try:
                            chunk = json.loads(data)
                            # Look for tool calls in the chunk
                            if 'choices' in chunk and len(chunk['choices']) > 0:
                                choice = chunk['choices'][0]
                                if 'delta' in choice and 'tool_calls' in choice['delta']:
                                    print(f"Tool call delta found: {choice['delta']['tool_calls']}")
                        except json.JSONDecodeError:
                            print(f"Failed to parse JSON: {data}")
        else:
            print(f"Error: {response.text}")
            
    except Exception as e:
        print(f"Exception occurred: {e}")

if __name__ == "__main__":
    print("Testing Grok API function calling...")
    print("=" * 50)
    
    # Test non-streaming first
    test_grok_function_call()
    
    print("\n" + "=" * 50)
    
    # Then test streaming
    test_grok_streaming()