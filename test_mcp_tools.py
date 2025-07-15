#!/usr/bin/env python3
"""
Test script to verify that the MCP server correctly exposes the parallel_read tool.
"""

import json
import subprocess
import sys
import tempfile
import os

def test_mcp_server_tools():
    """Test that the MCP server lists both codex and parallel_read tools."""
    
    # Create test input files
    with tempfile.TemporaryDirectory() as tmpdir:
        file1 = os.path.join(tmpdir, "test1.txt")
        file2 = os.path.join(tmpdir, "test2.txt")
        
        with open(file1, "w") as f:
            f.write("This is test file 1")
        with open(file2, "w") as f:
            f.write("This is test file 2")
    
        # Start the MCP server
        mcp_server = subprocess.Popen(
            ["cargo", "run", "--bin", "codex-mcp-server"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd="/Users/lbcheng/codex/codex-rs"
        )
        
        try:
            # Send initialize request
            init_request = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "protocolVersion": "1.0.0",
                    "capabilities": {},
                    "clientInfo": {
                        "name": "test-client",
                        "version": "1.0.0"
                    }
                }
            }
            
            mcp_server.stdin.write(json.dumps(init_request) + "\n")
            mcp_server.stdin.flush()
            
            # Read initialize response
            init_response = mcp_server.stdout.readline()
            print(f"Initialize response: {init_response.strip()}")
            
            # Send list tools request
            list_tools_request = {
                "jsonrpc": "2.0", 
                "id": 2,
                "method": "tools/list",
                "params": {}
            }
            
            mcp_server.stdin.write(json.dumps(list_tools_request) + "\n")
            mcp_server.stdin.flush()
            
            # Read tools list response
            tools_response = mcp_server.stdout.readline()
            print(f"Tools list response: {tools_response.strip()}")
            
            # Parse response and check for our tools
            response_data = json.loads(tools_response.strip())
            if "result" in response_data and "tools" in response_data["result"]:
                tools = response_data["result"]["tools"]
                tool_names = [tool["name"] for tool in tools]
                
                print(f"Available tools: {tool_names}")
                
                expected_tools = ["codex", "parallel_read"]
                for expected in expected_tools:
                    if expected in tool_names:
                        print(f"✅ Found tool: {expected}")
                    else:
                        print(f"❌ Missing tool: {expected}")
                        return False
                
                # Test parallel_read tool call
                parallel_read_request = {
                    "jsonrpc": "2.0",
                    "id": 3,
                    "method": "tools/call",
                    "params": {
                        "name": "parallel_read",
                        "arguments": {
                            "file-paths": [file1, file2],
                            "max-bytes-per-file": 1024,
                            "timeout-ms": 5000
                        }
                    }
                }
                
                print(f"Testing parallel_read with files: {[file1, file2]}")
                mcp_server.stdin.write(json.dumps(parallel_read_request) + "\n")
                mcp_server.stdin.flush()
                
                # Read parallel_read response
                read_response = mcp_server.stdout.readline()
                print(f"Parallel read response: {read_response.strip()}")
                
                # Parse and validate response
                read_data = json.loads(read_response.strip())
                if "result" in read_data:
                    result_content = read_data["result"]["content"][0]["text"]
                    result_json = json.loads(result_content)
                    
                    print(f"Read result: {result_json}")
                    
                    if result_json["success_count"] == 2 and result_json["error_count"] == 0:
                        print("✅ Parallel read test passed!")
                        return True
                    else:
                        print(f"❌ Parallel read test failed: {result_json}")
                        return False
                else:
                    print(f"❌ Invalid response format: {read_data}")
                    return False
            else:
                print(f"❌ Invalid tools list response: {response_data}")
                return False
                
        finally:
            mcp_server.terminate()
            mcp_server.wait()

if __name__ == "__main__":
    try:
        success = test_mcp_server_tools()
        sys.exit(0 if success else 1)
    except Exception as e:
        print(f"Test failed with error: {e}")
        sys.exit(1)