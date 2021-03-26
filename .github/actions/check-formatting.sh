#!/usr/bin/env bash
set -euo pipefail

run() {
  echo "$ $*" >&2
  "$@"
}

# Go to the repo root
cd "$(git rev-parse --show-toplevel)"

# Run the code formatter
run treefmt

# Check that there are no code changes
if [[ -n "$(git diff --stat)" ]]; then
  git status
  echo "FAIL: found some code changes"
  exit 1
fi
