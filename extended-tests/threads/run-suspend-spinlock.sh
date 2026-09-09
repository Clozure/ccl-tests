#!/usr/bin/env bash
# suspend-spinlock-repro.sh -- red/green driver for the suspend-vs-spinlock
# runtime deadlock (upstream issue #597, PR #634).
# Test widener: ../wideners/widen-guard-spinlock-window.patch
#
# Runs suspend-spinlock-deadlock.lisp under the given CCL and watches
# its heartbeats.  A run that completes every world-stop prints VERDICT
# GREEN and exits 0.  A run whose heartbeats stall for STALL_SECS is the
# deadlock: the driver captures a gdb backtrace of the spinning process
# (when gdb is present), prints VERDICT RED, and exits 42.  Any other
# failure exits with the underlying code and VERDICT ERROR.
#
# Usage:
#   CCL=/path/to/lx86cl64  [WIDEN=50000] [ITERS=3000] [WORKERS=4]
#     [STALL_SECS=30] [LOG=/path] bash extended-tests/threads/run-suspend-spinlock.sh
#
# WIDEN is only effective when the kernel carries the TEST-ONLY widener
# patch (0237); it widens every spin-lock held window so the race lands
# in seconds.  WIDEN=0 exercises the unwidened window (soak mode).
set -u

CCL="${CCL:?path to the CCL executable to test}"
WIDEN="${WIDEN:-50000}"
ITERS="${ITERS:-3000}"
WORKERS="${WORKERS:-4}"
STALL_SECS="${STALL_SECS:-30}"
HERE="$(cd "$(dirname "$0")" && pwd)"
REPRO_FILE="${REPRO_FILE:-$HERE/suspend-spinlock-repro.lisp}"
LOG="${LOG:-$(mktemp "${TMPDIR:-/tmp}/suspend-spinlock-repro-XXXXXX.log")}"

export CCL_SPINLOCK_WIDEN_LOOPS="$WIDEN"
export REPRO_ITERS="$ITERS"
export REPRO_WORKERS="$WORKERS"

"$CCL" ${CCL_IMAGE:+-I "$CCL_IMAGE"} --no-init --batch \
  --load "$REPRO_FILE" >"$LOG" 2>&1 &
PID=$!

last=""
stall=0
while kill -0 "$PID" 2>/dev/null; do
  sleep 2
  cur="$(grep -c HEARTBEAT "$LOG" 2>/dev/null || true)"
  if [ "$cur" = "$last" ]; then
    stall=$((stall + 2))
  else
    stall=0
    last="$cur"
  fi
  if [ "$stall" -ge "$STALL_SECS" ]; then
    echo "--- heartbeats stalled ${STALL_SECS}s; capturing signature"
    if command -v gdb >/dev/null 2>&1; then
      gdb -p "$PID" -batch \
          -ex "set pagination off" \
          -ex "thread apply all bt 8" 2>/dev/null | head -80
    else
      echo "(gdb not installed; no C backtrace)"
    fi
    kill -9 "$PID" 2>/dev/null
    wait "$PID" 2>/dev/null
    echo "last log lines:"; tail -4 "$LOG"
    echo "VERDICT RED (wedged after $(grep HEARTBEAT "$LOG" | tail -1 | awk '{print $2}' ) heartbeats; widen=$WIDEN iters=$ITERS workers=$WORKERS log=$LOG)"
    exit 42
  fi
done

wait "$PID"
rc=$?
tail -3 "$LOG"
if grep -q "REPRO-COMPLETE" "$LOG"; then
  echo "VERDICT GREEN (completed $ITERS world-stops; widen=$WIDEN workers=$WORKERS log=$LOG)"
  exit 0
fi
echo "VERDICT ERROR rc=$rc (log=$LOG)"
exit "$rc"
