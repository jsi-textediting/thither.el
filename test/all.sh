#!/bin/sh
# Usage: test/all.sh local|remote   (see test/run.sh for THITHER_SERVER)
# local also runs the session tests through a session daemon (run.sh session).
cd "$(dirname "$0")/.." || exit 1
for f in thither-test thither-fs-test thither-integration-test thither-use-package-test; do
  echo "=== $1 $f"
  timeout 280 sh test/run.sh "$1" "test/$f.el" 2>&1 | grep -E '^ +(FAILED|skipped)|^Ran' | cut -c1-200
done
if [ "$1" = local ]; then
  echo "=== session thither-fs-test"
  timeout 280 sh test/run.sh session test/thither-fs-test.el 2>&1 | grep -E '^ +(FAILED|skipped)|^Ran' | cut -c1-200
fi
