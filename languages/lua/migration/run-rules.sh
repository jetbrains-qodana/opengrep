#!/usr/bin/env bash
#
# Run the migration rules (rules/*/*.yaml against their annotated targets).
#
# usage: run-rules.sh [rules dir or file...]
# OPENGREP_CLI can point to another opengrep-cli executable (default: the one
# built in this repository).
#
set -eu -o pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
exe=${OPENGREP_CLI:-$root/bin/opengrep-cli}
if [[ $# -eq 0 ]]; then
  set -- "$here"/rules
fi
exec "$exe" test "$@"
