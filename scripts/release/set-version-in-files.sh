#! /usr/bin/env bash
#
# Rewrite the opengrep version string in every source file that carries it.
#
# This is the pure, side-effect-free core of the release bump: it only edits
# files (no git operations, no branch/release-id checks), so it can be reused
# both by scripts/release/bump and by CI (.github/actions/set-version) to stamp
# a version into an ephemeral checkout right before building.
#
# It operates on paths relative to the current directory, so it must be run
# from the repository root.
#
# NOTE: dune-project is intentionally NOT touched here. It is pinned to the
# semgrep version on purpose (bumping it invalidates the dune build cache, see
# the TODO in dune-project); scripts/release/bump updates it separately.
#
# Usage: set-version-in-files.sh <version>

set -eu

release="${1:-}"
if [ -z "$release" ]; then
  echo "Error: no version provided" >&2
  echo "Usage: $0 <version>" >&2
  exit 1
fi

case "$(uname -s)" in
  Linux)
    os_type=linux
    ;;
  *)
    os_type=Darwin
esac

sed_in_place() {
  if [ "$os_type" = linux ] || sed --version 2>/dev/null | grep GNU >/dev/null 2>&1; then
    sed -i -e "$@"
  else
    sed -i '' "$@"
  fi
}

# Escape characters that are special on the replacement side of sed. Normal
# semver versions contain none of these; PEP 440 local versions (e.g. the
# "+qodana" segment used by CI) do not either, but escaping keeps this robust.
esc=$(printf '%s' "$release" | sed -e 's/[\\/&]/\\&/g')

# coupling: scripts/release/bump git-adds exactly this set of files (plus
# dune-project), and the final guard there checks the same list.

# coupling: .github/workflows/pro-release.jsonnet
# coupling: OSS/.github/workflows/start-release.jsonnet
# The coupled files check for this exact path and string formatting. Version.ml
# is what `opengrep --version` prints.
sed_in_place 's/let version = ".*"/let version = "'"$esc"'"/g' src/core/Version.ml

sed_in_place 's/__VERSION__ = ".*"/__VERSION__ = "'"$esc"'"/g' cli/src/semgrep/__init__.py

# cli/setup.py drives the wheel name (opengrep-<version>.data), which the
# self-contained Nuitka binary builds resolve against, so it must match Version.ml.
sed_in_place 's/^    version=".*",$/    version="'"$esc"'",/g' cli/setup.py

sed_in_place \
  's/^    install_requires=\["semgrep==.*"\],$/    install_requires=["semgrep=='"$esc"'"],/g' \
  setup.py
sed_in_place 's/^    version=".*",$/    version="'"$esc"'",/g' setup.py
