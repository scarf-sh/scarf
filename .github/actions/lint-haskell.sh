#!/usr/bin/env bash
set -eou pipefail

for f in $(find srv/{src,bin} -name \*.hs);do
  echo "checking $f"
  ormolu -m check $f
done
