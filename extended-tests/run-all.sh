#!/usr/bin/env bash
# extended-tests/run-all.sh -- run every extended test.
#
# Usage:
#     bash extended-tests/run-all.sh /path/to/ccl
#     CCL=/path/to/ccl bash extended-tests/run-all.sh
#
# Works from any checkout: every path below is derived from this script's own
# location, so there is nothing to install and nothing to configure.
#
# ⛔ TWO PHASES, AND THEY ARE NOT THE SAME KIND OF TEST.
#
# Phase 1 -- the SUITE.  Ordinary tests: they load into one image, assert, and
# report.  A failure here is a real regression and this script exits non-zero.
#
# Phase 2 -- the REPRODUCERS.  Most reproduce deadlocks, and a deadlock
# cannot be detected from inside the image it deadlocks: when
# %SUSPEND-OTHER-THREADS wedges, every other thread is stopped, INCLUDING any
# watchdog you might write.  So each one runs in ITS OWN PROCESS under an
# EXTERNAL timeout, and a timeout IS the reproduction.  They are expected to
# reproduce -- that is, to fail -- on a lisp that does not yet carry the fix, so
# their result is reported but does NOT set the exit status.  A reproducer whose
# fix HAS landed is expected clean, and then a reproduction DOES fail this
# script.  See README.md for the commit that closed each one.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CCL="${1:-${CCL:-ccl}}"
# A TIMEOUT IS THE REPRODUCTION SIGNAL here, so this must comfortably exceed
# the slowest GREEN run or a slow machine reports a fixed defect as open.
# 180 could not: the two suspend reproducers default to 500000 cycles, and
# MEASURED on linuxarm64 with 24 workers on 2 cores they take 619 s
# (suspend-spinlock-deadlock) and 1053 s (suspend-spinlock-static-cons).
# Both would have been killed at 180 s and reported UNEXPECTED FAILURE.
# Lower it deliberately, or cut the work with REPRO_ITERS, but do not leave
# it below the slowest green you expect.
TIMEOUT_SECS="${TIMEOUT_SECS:-1800}"

command -v "$CCL" >/dev/null 2>&1 || [ -x "$CCL" ] || {
  echo "run-all: no CCL at '$CCL'.  Pass a path or set CCL=." >&2; exit 2; }

echo "=== extended tests ==="
echo "lisp   : $CCL"
"$CCL" --no-init --batch --quiet \
  -e '(progn (princ (lisp-implementation-version)) (terpri) (quit))' </dev/null 2>&1 \
  | tail -1 | sed 's/^/version: /'
echo

rc=0

echo "--- phase 1: the suite (a failure here is a regression) ---"
"$CCL" --no-init --batch -l "$HERE/load.lisp" -e '(run-extended-tests :exit t)' </dev/null
p1=$?
[ "$p1" = 0 ] || rc=1
echo "phase 1 exit: $p1"
echo

echo "--- phase 2: reproducers, one process each, ${TIMEOUT_SECS}s external timeout ---"
# EXPECTED STATE PER REPRODUCER.  `repro' means the defect is still open, so
# reproducing is the CORRECT outcome and does not fail this script.  When a fix
# lands the test stops reproducing, and THAT is reported loudly as UNEXPECTED-OK
# -- the signal to move the row to green in README.md and flip the state here.
#
# The state is a property of the LISP UNDER TEST, not of the test file.  Run a
# `clean' row against a lisp that predates the commit it names and it will
# report UNEXPECTED FAILURE and fail this script -- correctly, because against
# that lisp the defect is present.
#
# Do not flip a row from a merge alone.  Every `clean' below was measured on a
# kernel built from the commits it names.  One row that LOOKED ready to flip
# still reproduces, and only running it showed that.
#
# This is the XFAIL/XPASS pair every mature suite has (DejaGnu XPASS, lit XFAIL,
# pytest xfail(strict)).  RT's own *expected-failures* gives the XFAIL half but
# not the XPASS half, and it cannot help at all for a test that wedges the
# image, because a hung RT never reaches its report.
expected_state () {   # <name> -> repro | clean
  case "$1" in
    # Both flipped to `clean' on 2026-09-16, each MEASURED against a kernel
    # carrying the named commits, not inferred from them being merged.
    unbind-missed-suspend)     echo clean ;;   # 1606a83d + 53a509a6 (32-bit ARM)
    suspend-spinlock-deadlock) echo clean ;;   # 04f1e0ac + 088e706e
    #
    # Added by the maintainer as 17ec0a2 and MEASURED here before this row
    # was written: pin 2b7422e6, unwidened, 24 workers on 2 cores,
    # 500000 of 500000 cycles in 1053 s with 1.787G allocations.
    suspend-spinlock-static-cons) echo clean ;;  # 2c382468
    #
    # ⛔ THE ONLY `repro' ROW HERE, and the only file in threads/ that is not
    # part of the #597 family.  OPEN upstream as issue #639, so reproducing is
    # the CORRECT outcome and does not fail this script.  When it stops
    # reproducing, that is the UNEXPECTED-OK signal: the fix has landed, and
    # this row and the README move to green together.
    #
    # MEASURED on stock builds at pin 6526e21c before this row was written --
    # a row written from a merge or from a bug report, rather than from a run,
    # is the defect the `*)' fall-through below exists to stop:
    #
    #   linuxx8664  t3.small, 2 vCPU   10 s sleep took  31.0 s  (ratio  3.1)
    #                                  rc=42,   781,882 allocations
    #   linuxarm64  t4g.small, 2 vCPU  10 s sleep took 168.0 s  (ratio 16.8)
    #                                  rc=42, 6,474,520 allocations
    #
    # POSITIVE CONTROL, the same file with ONE variable changed -- the
    # allocation cut from 160016 bytes to 176 -- so that a red here cannot be
    # "this test always fails":
    #
    #   linuxx8664  rc=0, 10.6 s (ratio 1.1), 30,297,490 allocations
    #   linuxarm64  rc=0, 11.7 s (ratio 1.2), 27,336,462 allocations
    #
    # The control allocates 39x and 4x MORE OFTEN than the runs that fail, and
    # returns on time.  So the variable is object SIZE -- and therefore how
    # long each collection takes -- not whether another thread allocates.
    #
    # ⚠ A THROTTLED OR LOW-CORE BOX REPRODUCES THIS MORE WEAKLY, not more
    # strongly: fewer collections per second means less time lost per second.
    # Both cells above are burstable instances, so both are conservative.
    sleep-vs-alloc)            echo repro ;;   # OPEN -- issue #639
    #
    # ⚠ ONE UNEXPLAINED STALL, recorded here because it is not reproducible
    # and therefore cannot be a state.  On linuxarm64, 24 workers on 2 cores,
    # one run of suspend-spinlock-deadlock stopped emitting heartbeats at
    # 75,750 of 500,000 iterations and sat with 24 live threads for a further
    # nine minutes.  The stall began 73 s before the harness signalled it, so
    # the signal did not cause it.  A later run on the same kernel and image
    # completed all 500,000 (282 M allocations), and the maintainer measured
    # 500,000 clean on darwinarm64, linuxarm64 and darwinx8664.  So the state
    # is `clean' -- a stall is now a real failure and fails this script, which
    # is what we want if it recurs.
    #
    # UPDATE 2026-09-17: it has not recurred.  At pin 2b7422e6, which carries
    # the lisp half (2c382468), this reproducer ran 500000 of 500000 with
    # 285.8M allocations in 619 s.  We never captured a backtrace of OUR
    # stall, so we cannot say 2c382468 explains it -- only that the run is
    # clean on a lisp that carries it.
    #
    # A file with no row above has NO MEASURED STATE on this lisp.  Reporting
    # it `clean' hands an unmeasured test the best available outcome by
    # default, which contradicts the rule at the top of this function: the
    # state is a property of the LISP UNDER TEST, and nobody has run this one
    # here.  Measured 2026-09-17 -- suspend-spinlock-static-cons.lisp arrived
    # in this directory and the runner reported it `clean, as expected'
    # before anyone had run it once.
    *)                         echo unknown ;;
  esac
}
unexpected=0
for t in "$HERE"/threads/*.lisp; do
  case "$(basename "$t")" in *prelude*) continue ;; esac
  name="$(basename "$t" .lisp)"
  want="$(expected_state "$name")"
  printf '  %-30s ' "$name"
  timeout "$TIMEOUT_SECS" "$CCL" --no-init --batch -l "$t" </dev/null >"$HERE/.$name.out" 2>&1
  trc=$?
  # 0 = ran clean.  124 = external timeout, i.e. it wedged.  Anything else is
  # the test's own non-zero verdict: a reproducer exits 42 when it observes
  # the defect.
  #
  # ⚠ rc=0 IS NOT ENOUGH ON ITS OWN.  A lisp that fails to load its own
  # level-1 drops into the kernel debugger, prints a register dump, and still
  # EXITS 0.  Classifying that as `clean' turns a lisp that never ran the test
  # into a passing row -- measured 2026-09-16, and it is the one failure a test
  # runner must never have.  So a clean verdict also requires the test to have
  # SAID something: every reproducer prints a result or completion line, and
  # if none is present the run did not happen.
  case "$trc" in
    0)   if grep -qE 'REPRO-COMPLETE|-RESULT|VERDICT' "$HERE/.$name.out"; then
           got=clean
         else
           got=norun
         fi ;;
    124) got=repro ;;
    *)   got=repro ;;
  esac
  if [ "$got" = norun ]; then
    echo "DID NOT RUN -- exited 0 but printed no result line.  The lisp"
    echo "      probably failed to start (a failed level-1 load exits 0)."
    echo "      See .$name.out"
    rc=1
    continue
  fi
  if [ "$want" = unknown ]; then
    echo "NO EXPECTED STATE -- it ran and reported '$got', but no row in"
    echo "      expected_state() names this test, so nothing here has been"
    echo "      measured against this lisp.  Add a row naming the commit that"
    echo "      closes it, or 'repro' while the defect is open."
    unexpected=$((unexpected+1))
    rc=1
    continue
  fi
  if [ "$got" = "$want" ]; then
    if [ "$got" = repro ]; then echo "reproduced, as expected (rc=$trc)"
    else                        echo "clean, as expected" ; fi
  else
    unexpected=$((unexpected+1))
    if [ "$got" = clean ]; then
      echo "UNEXPECTED-OK -- it no longer reproduces.  The fix has landed:"
      echo "      move its row to green in README.md and set its state to clean."
    else
      echo "UNEXPECTED FAILURE (rc=$trc) -- expected clean.  See .$name.out"
      rc=1
    fi
  fi
done
[ "$unexpected" = 0 ] || echo "  ($unexpected reproducer(s) did not match their expected state)"

echo
echo "=== run-all exit $rc ==="
exit $rc
