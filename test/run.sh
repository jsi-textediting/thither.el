#!/bin/sh
# Usage: test/run.sh local|remote|session [test-file] [ert selector]
# Runs the ERT tests in batch Emacs against a thither-server.
#   local   THITHER_SERVER defaults to "thither-server --stdio" (on PATH);
#           set THITHER_SERVER_BIN to use another binary.
#   remote  THITHER_SERVER must be the whole command, for example
#           THITHER_SERVER="ssh -T remote-box thither/thither-server --stdio"
#           THITHER_SERVER="plink.exe -ssh -batch -T remote-box thither/thither-server --stdio"
#   session like local, through a session daemon "el_<pid>" (--session) with
#           temporary XDG_RUNTIME_DIR / XDG_STATE_HOME; the daemon is killed
#           and the directories removed afterwards.  Default selector:
#           the session tests.
cd "$(dirname "$0")/.." || exit 1
run() {
  emacs -Q --batch -L . -l "${1:-test/thither-fs-test.el}" --eval "(setq ert-batch-backtrace-right-margin 150 ert-batch-print-length 8 ert-batch-print-level 4)" --eval "(ert-run-tests-batch-and-exit '${2:-t})"
}
case "$1" in
  remote)
    if [ -z "$THITHER_SERVER" ]; then
      echo "test/run.sh remote: set THITHER_SERVER to the command that starts the server" >&2
      exit 2
    fi ;;
  session)
    bin=${THITHER_SERVER_BIN:-thither-server}
    name=el_$$
    base=$(mktemp -d) || exit 1
    mkdir -m 700 "$base/run" "$base/state"
    export XDG_RUNTIME_DIR="$base/run" XDG_STATE_HOME="$base/state"
    export THITHER_SERVER="$bin --stdio --session $name"
    run "$2" "${3:-\"thither-fs-session\"}"
    status=$?
    "$bin" --session "$name" --kill >/dev/null 2>&1
    rm -rf "$base"
    exit $status ;;
  *) export THITHER_SERVER="${THITHER_SERVER:-${THITHER_SERVER_BIN:-thither-server} --stdio}" ;;
esac
run "$2" "$3"
