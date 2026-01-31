#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Uses env vars: OPENGREP_RULES_PATH, SCAN_ON_MISS, SKIP_TAINT, THREADS, DIAGNOSTIC_TIMEOUT_SECS, RUN_TAG, ONLY_FILES_LIST, NOPCOMMERCE_PATH
mvn -q -DskipTests compile
mvn -q -DskipTests exec:java
