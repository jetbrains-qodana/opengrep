# LSP Document Version Tracking Tests

This directory contains tests for the opengrep LSP server's document version tracking feature.

## Test Files

- `test_file.py` - Sample Python file with security issues for Semgrep to detect
- `test_lsp_versions.py` - Comprehensive test suite for document version tracking

## Running the Tests

```bash
cd testlsp
python3 test_lsp_versions.py
```

The tests will automatically:
1. Start the opengrep LSP server (`../bin/opengrep.bc`)
2. Initialize the server with the test workspace
3. Run three test scenarios
4. Report results
5. Clean up and stop the server

## Test Scenarios

### Test 1: Basic Version Tracking
- Sends a single `textDocument/didOpen` with version 9
- Verifies the `textDocument/publishDiagnostics` response includes version 9

### Test 2: Multiple Sequential Versions
- Sends multiple `didOpen` requests with different versions (1, 5, 10, 42, 100)
- Each request waits for response before sending the next
- Verifies each response has the correct version

### Test 3: Rapid Fire Requests
- Sends 5 `didOpen` requests rapidly with versions 10-14
- No delays between requests
- Verifies all 5 responses come back with their respective versions
- Tests for race conditions in version tracking

### Test 4: New Files Immediate Scan (Target Cache Fix)
- Creates 3 new Python files with security issues:
  - SQL injection vulnerability
  - Command injection vulnerability
  - Hardcoded credentials
- Immediately sends `didOpen` for all files (no delay)
- Verifies all files are scanned and diagnostics returned
- Tests the target cache update fix where newly created files weren't being scanned
- Confirms the fix in `Scan_helpers.ml:255-257` properly refreshes targets

## Expected Results

All tests should pass with output like:
```
============================================================
📊 TEST SUMMARY
============================================================
Passed: 4/4
✅ ALL TESTS PASSED!
```

## Implementation Details

The LSP server tracks document versions by:
1. Capturing the version from `textDocument/didOpen` notifications
2. Storing versions in a hashtable indexed by file path
3. Immediately capturing the version when a scan is initiated (before async operations)
4. Including the version in `PublishDiagnosticsParams` responses

This ensures that even with rapid-fire requests, each diagnostic response is associated with the correct document version.

## Requirements

- Python 3.x
- opengrep bytecode binary at `../bin/opengrep.bc`
- Working opengrep installation with rules configured
