#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Uses env vars: OPENGREP_RULES_PATH, SCAN_ON_MISS, SKIP_TAINT, DISABLE_TARGET_CACHE, SANE_STDERR,
# THREADS, DIAGNOSTIC_TIMEOUT_SECS, RUN_TAG, ONLY_FILES_LIST, NOPCOMMERCE_PATH, VERSION_MATCH,
# MAX_FILES, OPENGREP_LSP_CMD, OPENGREP_LSP_TIMING, PER_FILE_LOG
# Pass flags to Java via script args, e.g. ./run-repro.sh --version-match=true
mvn -q -DskipTests compile
EXEC_ARGS=""
if [ "$#" -gt 0 ]; then
  EXEC_ARGS="$*"
fi
mvn -q -DskipTests exec:java -Dexec.args="$EXEC_ARGS"
