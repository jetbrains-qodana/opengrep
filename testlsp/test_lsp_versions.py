#!/usr/bin/env python3
"""Test script for LSP document version tracking."""

import json
import subprocess
import sys
import os
import time
from pathlib import Path
from threading import Thread, Lock
from queue import Queue

class LSPClient:
    def __init__(self, server_path):
        self.server_path = server_path
        self.process = None
        self.message_id = 0
        self.responses = {}
        self.notifications = []
        self.lock = Lock()
        self.notification_queue = Queue()

    def start(self):
        """Start the LSP server process."""
        print(f"Starting LSP server: {self.server_path}")
        self.process = subprocess.Popen(
            [self.server_path, "lsp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=0
        )

        # Start reader threads for both stdout and stderr
        self.reader_thread = Thread(target=self._read_loop, daemon=True)
        self.reader_thread.start()

        # Start stderr drain thread to prevent blocking
        self.stderr_thread = Thread(target=self._drain_stderr, daemon=True)
        self.stderr_thread.start()

        # Give server time to start
        time.sleep(0.5)

    def _drain_stderr(self):
        """Continuously drain stderr to prevent process blocking."""
        while True:
            try:
                line = self.process.stderr.readline()
                if not line:
                    break
                # Optionally log stderr output for debugging
                # print(f"[stderr] {line.decode('utf-8', errors='ignore').rstrip()}")
            except Exception as e:
                print(f"Error draining stderr: {e}")
                break

    def _read_loop(self):
        """Read messages from server."""
        buffer = b""
        while True:
            try:
                chunk = self.process.stdout.read(1)
                if not chunk:
                    break
                buffer += chunk

                # Check for complete message
                if b"\r\n\r\n" in buffer:
                    header_end = buffer.index(b"\r\n\r\n")
                    header = buffer[:header_end].decode('utf-8')

                    # Parse content length
                    content_length = None
                    for line in header.split("\r\n"):
                        if line.startswith("Content-Length:"):
                            content_length = int(line.split(":")[1].strip())
                            break

                    if content_length:
                        # Read the exact content
                        content_start = header_end + 4
                        while len(buffer) < content_start + content_length:
                            chunk = self.process.stdout.read(1)
                            if not chunk:
                                return
                            buffer += chunk

                        content = buffer[content_start:content_start + content_length]
                        buffer = buffer[content_start + content_length:]

                        try:
                            message = json.loads(content.decode('utf-8'))
                            self._handle_message(message)
                        except json.JSONDecodeError as e:
                            print(f"Failed to decode JSON: {e}")
                            print(f"Content: {content}")
            except Exception as e:
                print(f"Error in read loop: {e}")
                break

    def _handle_message(self, message):
        """Handle incoming message from server."""
        with self.lock:
            if "id" in message and "result" in message:
                # Response to our request
                self.responses[message["id"]] = message
            elif "method" in message:
                # Notification from server
                self.notifications.append(message)
                self.notification_queue.put(message)

                # Print notifications for debugging
                if message["method"] == "textDocument/publishDiagnostics":
                    params = message.get("params", {})
                    version = params.get("version")
                    uri = params.get("uri", "")
                    diag_count = len(params.get("diagnostics", []))
                    print(f"  📨 Received publishDiagnostics: version={version}, diagnostics={diag_count}, uri={Path(uri).name}")
                elif message["method"] == "semgrep/handleAST":
                    params = message.get("params", {})
                    version = params.get("version")
                    uri = params.get("uri", "")
                    value_len = len(params.get("value", ""))
                    print(f"  📨 Received handleAST: version={version}, value_len={value_len}, uri={Path(uri).name if uri else 'unknown'}")

    def send_request(self, method, params):
        """Send a request and return message ID."""
        self.message_id += 1
        msg_id = self.message_id

        message = {
            "jsonrpc": "2.0",
            "id": msg_id,
            "method": method,
            "params": params
        }

        self._send_message(message)
        return msg_id

    def send_notification(self, method, params):
        """Send a notification (no response expected)."""
        message = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        }

        self._send_message(message)

    def _send_message(self, message):
        """Send a JSON-RPC message to server."""
        content = json.dumps(message).encode('utf-8')
        header = f"Content-Length: {len(content)}\r\n\r\n".encode('utf-8')

        self.process.stdin.write(header + content)
        self.process.stdin.flush()

    def wait_for_response(self, msg_id, timeout=30):
        """Wait for a response with given message ID."""
        start = time.time()
        while time.time() - start < timeout:
            with self.lock:
                if msg_id in self.responses:
                    return self.responses[msg_id]
            time.sleep(0.1)
        raise TimeoutError(f"No response for message ID {msg_id}")

    def wait_for_notification(self, method, timeout=30):
        """Wait for a specific notification, discarding others."""
        start = time.time()
        seen_methods = set()
        while time.time() - start < timeout:
            try:
                notif = self.notification_queue.get(timeout=0.1)
                notif_method = notif.get("method")
                seen_methods.add(notif_method)
                if notif_method == method:
                    return notif
                # Discard notifications we're not waiting for
                # (don't put them back in the queue)
            except:
                pass
        # Include what we saw in the timeout error for debugging
        raise TimeoutError(f"No notification for method {method}. Saw: {seen_methods}")

    def clear_notifications(self):
        """Clear all stored notifications."""
        with self.lock:
            self.notifications.clear()
        # Clear queue
        while not self.notification_queue.empty():
            try:
                self.notification_queue.get_nowait()
            except:
                break

    def stop(self):
        """Stop the LSP server."""
        if self.process:
            try:
                self.send_notification("exit", {})
                time.sleep(0.5)
            except:
                pass
            try:
                self.process.terminate()
                self.process.wait(timeout=5)
            except:
                self.process.kill()

def test_basic_version_tracking(client, test_file_path):
    """Test basic document version tracking."""
    print("\n" + "="*60)
    print("TEST 1: Basic Version Tracking")
    print("="*60)

    test_version = 9

    print(f"📤 Sending didOpen with version {test_version}")
    client.send_notification("textDocument/didOpen", {
        "textDocument": {
            "uri": f"file://{test_file_path}",
            "languageId": "python",
            "version": test_version,
            "text": test_file_path.read_text()
        }
    })

    print("⏳ Waiting for publishDiagnostics...")
    try:
        notif = client.wait_for_notification("textDocument/publishDiagnostics", timeout=30)
        params = notif.get("params", {})
        returned_version = params.get("version")
        diagnostics = params.get("diagnostics", [])

        print(f"✅ Received publishDiagnostics")
        print(f"   Expected version: {test_version}")
        print(f"   Returned version: {returned_version}")
        print(f"   Diagnostics count: {len(diagnostics)}")

        if returned_version == test_version:
            print("✅ TEST PASSED: Version matches!")
            return True
        else:
            print(f"❌ TEST FAILED: Version mismatch! Expected {test_version}, got {returned_version}")
            return False
    except TimeoutError:
        print("❌ TEST FAILED: Timeout waiting for publishDiagnostics")
        return False

def test_multiple_versions(client, test_file_path):
    """Test with multiple different versions."""
    print("\n" + "="*60)
    print("TEST 2: Multiple Sequential Versions")
    print("="*60)

    test_versions = [1, 5, 10, 42, 100]
    all_passed = True

    for version in test_versions:
        client.clear_notifications()

        print(f"\n📤 Sending didOpen with version {version}")
        client.send_notification("textDocument/didOpen", {
            "textDocument": {
                "uri": f"file://{test_file_path}",
                "languageId": "python",
                "version": version,
                "text": test_file_path.read_text()
            }
        })

        print("⏳ Waiting for publishDiagnostics...")
        try:
            notif = client.wait_for_notification("textDocument/publishDiagnostics", timeout=30)
            params = notif.get("params", {})
            returned_version = params.get("version")

            print(f"   Expected: {version}, Returned: {returned_version}")

            if returned_version == version:
                print(f"   ✅ Version {version} matched")
            else:
                print(f"   ❌ Version mismatch for {version}!")
                all_passed = False
        except TimeoutError:
            print(f"   ❌ Timeout for version {version}")
            all_passed = False

        # Small delay between requests
        time.sleep(1)

    if all_passed:
        print("\n✅ TEST PASSED: All versions matched!")
    else:
        print("\n❌ TEST FAILED: Some versions did not match")

    return all_passed

def test_rapid_fire_versions(client, test_file_path):
    """Test rapid-fire requests with different versions."""
    print("\n" + "="*60)
    print("TEST 3: Rapid Fire Requests")
    print("="*60)

    test_versions = [10, 11, 12, 13, 14]

    client.clear_notifications()

    print(f"📤 Sending {len(test_versions)} rapid-fire didOpen requests...")
    for version in test_versions:
        client.send_notification("textDocument/didOpen", {
            "textDocument": {
                "uri": f"file://{test_file_path}",
                "languageId": "python",
                "version": version,
                "text": test_file_path.read_text()
            }
        })
        print(f"   Sent version {version}")

    print(f"\n⏳ Waiting for {len(test_versions)} publishDiagnostics responses...")

    received_versions = []
    timeout_per_response = 30

    for i in range(len(test_versions)):
        try:
            notif = client.wait_for_notification("textDocument/publishDiagnostics", timeout=timeout_per_response)
            params = notif.get("params", {})
            returned_version = params.get("version")
            received_versions.append(returned_version)
            print(f"   Received version: {returned_version}")
        except TimeoutError:
            print(f"   ⏰ Timeout waiting for response {i+1}")
            break

    print(f"\n📊 Summary:")
    print(f"   Sent versions: {test_versions}")
    print(f"   Received versions: {received_versions}")

    # Check if all versions were received (order doesn't matter)
    if sorted(received_versions) == sorted(test_versions):
        print("✅ TEST PASSED: All versions received correctly!")
        return True
    else:
        print("❌ TEST FAILED: Version mismatch in rapid-fire test")
        return False

def test_new_files_immediate_scan(client, workspace_root):
    """Test creating new files and immediately scanning them (tests target cache fix)."""
    print("\n" + "="*60)
    print("TEST 4: New Files Immediate Scan (Target Cache Fix)")
    print("="*60)

    # Create test files with security issues
    test_files = []
    test_code_templates = [
        # File 1: SQL injection
        """
import sqlite3

def unsafe_query(user_input):
    conn = sqlite3.connect('database.db')
    cursor = conn.cursor()
    # Vulnerable to SQL injection
    query = "SELECT * FROM users WHERE name = '" + user_input + "'"
    cursor.execute(query)
    return cursor.fetchall()
""",
        # File 2: Command injection
        """
import os

def run_command(user_input):
    # Vulnerable to command injection
    os.system("echo " + user_input)
""",
        # File 3: Hardcoded credentials
        """
import requests

def connect_to_api():
    # Hardcoded credentials
    api_key = "sk_live_1234567890abcdefghijklmnop"
    headers = {"Authorization": f"Bearer {api_key}"}
    response = requests.get("https://api.example.com/data", headers=headers)
    return response.json()
""",
    ]

    print(f"📝 Creating {len(test_code_templates)} new test files...")
    for i, code in enumerate(test_code_templates):
        file_path = workspace_root / f"temp_test_{i}.py"
        file_path.write_text(code)
        test_files.append(file_path)
        print(f"   Created: {file_path.name}")

    client.clear_notifications()

    print(f"\n📤 Immediately sending didOpen for all {len(test_files)} files...")
    # Send didOpen immediately without any delay (tests cache update)
    for i, file_path in enumerate(test_files):
        version = 100 + i
        client.send_notification("textDocument/didOpen", {
            "textDocument": {
                "uri": f"file://{file_path}",
                "languageId": "python",
                "version": version,
                "text": file_path.read_text()
            }
        })
        print(f"   Sent didOpen for {file_path.name} (version {version})")

    print(f"\n⏳ Waiting for publishDiagnostics responses...")

    results = []
    timeout_per_file = 30

    for i, file_path in enumerate(test_files):
        try:
            notif = client.wait_for_notification("textDocument/publishDiagnostics", timeout=timeout_per_file)
            params = notif.get("params", {})
            uri = params.get("uri", "")
            version = params.get("version")
            diagnostics = params.get("diagnostics", [])

            filename = Path(uri).name if uri else "unknown"
            has_findings = len(diagnostics) > 0

            results.append({
                "file": filename,
                "version": version,
                "diagnostics_count": len(diagnostics),
                "has_findings": has_findings
            })

            status = "✅" if has_findings else "⚠️"
            print(f"   {status} {filename}: version={version}, diagnostics={len(diagnostics)}")

        except TimeoutError:
            print(f"   ⏰ Timeout waiting for {test_files[i].name}")
            results.append({
                "file": test_files[i].name,
                "version": None,
                "diagnostics_count": 0,
                "has_findings": False
            })

    # Cleanup test files
    print(f"\n🧹 Cleaning up test files...")
    for file_path in test_files:
        try:
            file_path.unlink()
            print(f"   Deleted: {file_path.name}")
        except:
            pass

    # Analyze results
    print(f"\n📊 Results Summary:")
    files_with_findings = sum(1 for r in results if r["has_findings"])
    files_scanned = len([r for r in results if r["version"] is not None])

    print(f"   Files created: {len(test_files)}")
    print(f"   Files scanned: {files_scanned}")
    print(f"   Files with findings: {files_with_findings}")

    # We expect at least some files to have findings
    # (depends on rules configured, but SQL injection is common)
    if files_scanned == len(test_files):
        if files_with_findings > 0:
            print(f"✅ TEST PASSED: New files scanned successfully, {files_with_findings} had findings!")
            return True
        else:
            print("⚠️  TEST PASSED (with warning): All files scanned but no findings (rules may not be configured)")
            return True
    else:
        print(f"❌ TEST FAILED: Only {files_scanned}/{len(test_files)} files were scanned")
        print("   This suggests the target cache was not updated properly.")
        return False

def test_webgoat_csharp_files(client):
    """Test scanning all C# files from WebGoat.NET directory."""
    print("\n" + "="*60)
    print("TEST 5: WebGoat.NET C# Files Scan")
    print("="*60)

    # Find all .cs files recursively
    webgoat_path = Path.home() / "Qodana" / "WebGoat.NET"

    if not webgoat_path.exists():
        print(f"❌ WebGoat.NET directory not found at: {webgoat_path}")
        print("   Skipping this test.")
        return True  # Don't fail if directory doesn't exist

    print(f"📂 Scanning directory: {webgoat_path}")

    # If no client provided, create our own with WebGoat.NET as workspace
    own_client = False
    cs_files = list(webgoat_path.rglob("*.cs"))

    if not cs_files:
        print("⚠️  No .cs files found")
        return True

    print(f"📝 Found {len(cs_files)} C# files")

    print(f"\n📤 Processing files sequentially (send didOpen → wait for publishDiagnostics)...")

    results = []
    received_count = 0
    timeout_per_file = 30
    total_diagnostics = 0
    files_with_findings = 0

    start_time = time.time()

    # Process files sequentially
    for i, file_path in enumerate(cs_files):
        version = 200 + i

        try:
            # Read file content
            file_content = file_path.read_text(encoding='utf-8', errors='ignore')

            # Clear notifications before sending didOpen
            client.clear_notifications()

            # Send didOpen
            client.send_notification("textDocument/didOpen", {
                "textDocument": {
                    "uri": f"file://{file_path}",
                    "languageId": "csharp",
                    "version": version,
                    "text": file_content
                }
            })

            # Wait for publishDiagnostics response
            notif = client.wait_for_notification("textDocument/publishDiagnostics", timeout=timeout_per_file)
            params = notif.get("params", {})
            uri = params.get("uri", "")
            returned_version = params.get("version")
            diagnostics = params.get("diagnostics", [])

            filename = Path(uri).name if uri else "unknown"
            diag_count = len(diagnostics)
            total_diagnostics += diag_count

            if diag_count > 0:
                files_with_findings += 1

            results.append({
                "file": filename,
                "version": returned_version,
                "diagnostics_count": diag_count
            })

            received_count += 1

            if (i + 1) % 20 == 0:
                print(f"   Processed {i + 1}/{len(cs_files)} files (total diagnostics so far: {total_diagnostics})...")

        except TimeoutError:
            print(f"   ⏰ Timeout waiting for response for file {i+1}/{len(cs_files)}: {file_path.name}")
            break
        except Exception as e:
            print(f"   ⚠️  Error processing {file_path.name}: {e}")
            continue

    elapsed_time = time.time() - start_time

    # Print summary
    print(f"\n📊 Results Summary:")
    print(f"   Files found: {len(cs_files)}")
    print(f"   Responses received: {received_count}/{len(cs_files)}")
    print(f"   Files with findings: {files_with_findings}")
    print(f"   Total diagnostics: {total_diagnostics}")
    print(f"   Time elapsed: {elapsed_time:.2f} seconds")

    if received_count > 0:
        avg_time = elapsed_time / received_count
        print(f"   Average time per file: {avg_time:.3f} seconds")

    # Show top files with most findings
    if files_with_findings > 0:
        results_with_findings = [r for r in results if r["diagnostics_count"] > 0]
        results_with_findings.sort(key=lambda x: x["diagnostics_count"], reverse=True)

        print(f"\n   Top files with findings:")
        for r in results_with_findings[:10]:
            print(f"      {r['file']}: {r['diagnostics_count']} diagnostics")

    # Test passes if we received responses for most files (>90%)
    success_rate = received_count / len(cs_files) if cs_files else 0
    test_passed = success_rate >= 0.9

    if test_passed:
        print(f"\n✅ TEST PASSED: Received {received_count}/{len(cs_files)} responses ({success_rate*100:.1f}%)")
    else:
        print(f"\n❌ TEST FAILED: Only received {received_count}/{len(cs_files)} responses ({success_rate*100:.1f}%)")

    # Clean up if we created our own client
    if own_client:
        print("🛑 Stopping dedicated LSP server...")
        client.stop()

    return test_passed

def main():
    # Setup paths
    script_dir = Path(__file__).parent
    test_file = script_dir / "test_file.py"
    server_path = script_dir.parent / "bin" / "opengrep.bc"
    workspace_root = script_dir

    if not server_path.exists():
        print(f"❌ Server not found at: {server_path}")
        return 1

    if not test_file.exists():
        print(f"❌ Test file not found at: {test_file}")
        return 1

    print("🚀 Starting LSP Version Tracking Tests")
    print(f"   Server: {server_path}")
    print(f"   Test file: {test_file}")
    print(f"   Workspace: {workspace_root}")

    # Start client
    client = LSPClient(str(server_path))

    try:
        client.start()

        # Initialize
        print("\n📡 Initializing LSP server...")
        init_id = client.send_request("initialize", {
            "processId": os.getpid(),
            "rootUri": f"file://{workspace_root}",
            "capabilities": {
                "textDocument": {
                    "synchronization": {
                        "didOpen": True
                    }
                }
            },
            "initializationOptions": {
                "scan": {
                    "configuration": ["auto"],
                    "onlyGitDirty": False
                }
            }
        })

        response = client.wait_for_response(init_id, timeout=30)
        print("✅ Server initialized")

        # Send initialized notification
        client.send_notification("initialized", {})

        # Wait for rules to load
        print("⏳ Waiting for rules to load...")
        time.sleep(5)

        # Run tests
        results = []
        results.append(test_basic_version_tracking(client, test_file))
        results.append(test_multiple_versions(client, test_file))
        results.append(test_rapid_fire_versions(client, test_file))
        results.append(test_new_files_immediate_scan(client, workspace_root))
        # WebGoat test creates its own LSP server with WebGoat.NET workspace
        results.append(test_webgoat_csharp_files(client))

        # Summary
        print("\n" + "="*60)
        print("📊 TEST SUMMARY")
        print("="*60)
        passed = sum(results)
        total = len(results)
        print(f"Passed: {passed}/{total}")

        if all(results):
            print("✅ ALL TESTS PASSED!")
            return 0
        else:
            print("❌ SOME TESTS FAILED")
            return 1

    except Exception as e:
        print(f"\n❌ Error during test: {e}")
        import traceback
        traceback.print_exc()
        return 1
    finally:
        print("\n🛑 Stopping LSP server...")
        client.stop()

if __name__ == "__main__":
    sys.exit(main())
