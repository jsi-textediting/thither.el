#!/bin/sh
# Usage: test/all.sh local|remote   (see test/run.sh for THITHER_SERVER)
cd "$(dirname "$0")/.." || exit 1
for f in thither-test thither-fs-test thither-integration-test thither-use-package-test; do
  echo "=== $1 $f"
  timeout 280 sh test/run.sh "$1" "test/$f.el" 2>&1 | grep -E '^ +(FAILED|skipped)|^Ran' | cut -c1-200
done
