#!/bin/sh
# Usage: test/run.sh local|remote [test-file] [ert selector]
# Runs the ERT tests in batch Emacs against a thither-server.
#   local   THITHER_SERVER defaults to "thither-server --stdio" (on PATH);
#           set THITHER_SERVER_BIN to use another binary.
#   remote  THITHER_SERVER must be the whole command, for example
#           THITHER_SERVER="ssh -T remote-box thither/thither-server --stdio"
#           THITHER_SERVER="plink.exe -ssh -batch -T remote-box thither/thither-server --stdio"
cd "$(dirname "$0")/.." || exit 1
case "$1" in
  remote)
    if [ -z "$THITHER_SERVER" ]; then
      echo "test/run.sh remote: set THITHER_SERVER to the command that starts the server" >&2
      exit 2
    fi ;;
  *) export THITHER_SERVER="${THITHER_SERVER:-${THITHER_SERVER_BIN:-thither-server} --stdio}" ;;
esac
exec emacs -Q --batch -L . -l "${2:-test/thither-fs-test.el}" --eval "(setq ert-batch-backtrace-right-margin 150 ert-batch-print-length 8 ert-batch-print-level 4)" --eval "(ert-run-tests-batch-and-exit '${3:-t})"
