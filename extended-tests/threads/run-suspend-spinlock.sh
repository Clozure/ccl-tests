#!/usr/bin/env bash
# suspend-spinlock-repro.sh -- red/green driver for the suspend-vs-spinlock
# runtime deadlock (portable-patches/0235 + 0236; test-only widener 0237).
#
# ⛔ WHAT WAS WRONG WITH THIS SCRIPT, MEASURED 2026-09-10.  It reported
# VERDICT GREEN for a full session against a build that a working harness
# wedges in 90 seconds.  Six defects, each sufficient on its own:
#
#   1. The green line read `completed $ITERS world-stops' -- it echoed the
#      ENVIRONMENT VARIABLE and measured nothing.  A run that did 200
#      iterations and stopped printed "completed 10000".  That is a claim
#      formatted as a result, and it is the whole reflex-10 failure.
#   2. WIDEN defaulted to 50000, so the DEFAULT mode was the artificial
#      widener.  A red obtained that way cannot support any statement about
#      a shipped build, which is the only statement worth making.
#   3. ITERS defaulted to 3000.  Measured wedges need 51,750-103,250 cycles.
#      The default was ~30x too small; a default invocation could not fail.
#   4. WORKERS defaulted to 4, against the 24 the mechanism needs.
#   5. There was no exposure FLOOR: GREEN printed however little ran.
#   6. Nothing checked the victims were alive.  A worker set that died on
#      its first allocation is indistinguishable from a clean pass.
#
# So: the green path now REFUSES unless the run measurably reached past the
# window, with live workers that measurably allocated.  Anything short of
# that is INCONCLUSIVE, which is a different thing from a pass.
#
# Usage:
#   CCL=/path/to/lx86cl64 [CCL_IMAGE=/path] [WIDEN=0] [ITERS=500000]
#     [WORKERS=24] [STALL_SECS=25] [RED_FLOOR=400000] [LOG=/path] \
#     bash extended-tests/threads/run-suspend-spinlock.sh
#
# Exit: 0 GREEN (survived past the floor) | 42 RED (wedged) |
#       3 INCONCLUSIVE (ran clean but under the floor, or victims not proven)
set -u

CCL="${CCL:?path to the CCL executable to test}"
WIDEN="${WIDEN:-0}"                 # 0 = the unwidened window; the only mode
ITERS="${ITERS:-500000}"            # that can support a claim about shipping
WORKERS="${WORKERS:-24}"
STALL_SECS="${STALL_SECS:-25}"
# ⛔ SET FROM MEASUREMENT, NOT JUDGEMENT.  The wedge point is stochastic over
# an ORDER OF MAGNITUDE.  Ten reds measured 2026-09-10 on this hardware:
#   18,500  28,500  39,500  51,750  70,500  88,250  103,250  137,000
#   164,500  200,250
# and one run WITHOUT the fix survived to 199,750 before being stopped -- a
# survivor sitting at the top of that same distribution.  So a floor of
# 150,000 (this script's first value) would have certified a build that
# wedges.  The floor is twice the longest observed red.  Raise it, never
# lower it, and only against measured reds.
RED_FLOOR="${RED_FLOOR:-400000}"
HERE="$(cd "$(dirname "$0")" && pwd)"
LOG="${LOG:-$(mktemp "${TMPDIR:-/tmp}/suspend-spinlock-repro-XXXXXX.log")}"

# The default MUST name a file that exists.  This defaulted to
# suspend-spinlock-repro.lisp, which is not in this directory, so the
# documented invocation handed CCL a --load on a missing path and the run
# died with a reader error that looked like a lisp fault.
REPRO_FILE="${REPRO_FILE:-$HERE/suspend-spinlock-deadlock.lisp}"
if [ ! -f "$REPRO_FILE" ]; then
  echo "VERDICT ERROR: no such reproducer: $REPRO_FILE" >&2
  exit 2
fi

export CCL_SPINLOCK_WIDEN_LOOPS="$WIDEN"
export REPRO_ITERS="$ITERS"
export REPRO_WORKERS="$WORKERS"

# MEASURE, never assume: every number in a verdict comes from the log.
measured_cycles() { grep '^HEARTBEAT ' "$LOG" 2>/dev/null | tail -1 | awk '{print $2+0}'; }
measured_allocs() { grep '^HEARTBEAT ' "$LOG" 2>/dev/null | tail -1 | sed -n 's/.*allocs=\([0-9]*\).*/\1/p'; }
measured_live()   { grep '^HEARTBEAT ' "$LOG" 2>/dev/null | tail -1 | sed -n 's/.*live=\([0-9]*\).*/\1/p'; }
started_threads() { grep '^REPRO-THREADS ' "$LOG" 2>/dev/null | tail -1 | awk '{print $2+0}'; }

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
      echo "--- threads by state ---"
      awk '{print $3}' /proc/"$PID"/task/*/stat 2>/dev/null | sort | uniq -c
    else
      echo "(gdb not installed; no C backtrace)"
    fi
    kill -9 "$PID" 2>/dev/null
    wait "$PID" 2>/dev/null
    echo "last log lines:"; tail -4 "$LOG"
    echo "VERDICT RED (wedged at $(measured_cycles) cycles, allocs=$(measured_allocs), live=$(measured_live); widen=$WIDEN workers=$WORKERS log=$LOG)"
    exit 42
  fi
done

wait "$PID"
rc=$?
tail -3 "$LOG"

cycles="$(measured_cycles)";  cycles="${cycles:-0}"
allocs="$(measured_allocs)";  allocs="${allocs:-0}"
threads="$(started_threads)"; threads="${threads:-0}"

if ! grep -q "REPRO-COMPLETE" "$LOG"; then
  echo "VERDICT ERROR rc=$rc (reached $cycles cycles; log=$LOG)"
  exit "$rc"
fi

# A completed run is only a PASS if it actually exercised the mechanism.
if [ "$threads" -lt "$WORKERS" ] || [ "$allocs" -lt 1000 ]; then
  echo "VERDICT INCONCLUSIVE (run completed but the victims are not proven:" \
       "REPRO-THREADS=$threads of $WORKERS, allocs=$allocs; log=$LOG)"
  exit 3
fi
if [ "$cycles" -lt "$RED_FLOOR" ]; then
  echo "VERDICT INCONCLUSIVE (clean to $cycles cycles, under the $RED_FLOOR" \
       "floor; wedges measured 2026-09-10 span 18500 to 200250, so this" \
       "proves nothing. Raise ITERS; log=$LOG)"
  exit 3
fi
echo "VERDICT GREEN (clean to $cycles cycles > $RED_FLOOR floor, allocs=$allocs," \
     "threads=$threads; widen=$WIDEN workers=$WORKERS log=$LOG)"
exit 0
