#!/usr/bin/env bash
# extended-tests/run-all.sh -- run every extended test.
#
# Usage:
#     bash extended-tests/run-all.sh /path/to/ccl
#     CCL=/path/to/ccl bash extended-tests/run-all.sh
#     bash extended-tests/run-all.sh --list   # expected states, runs nothing
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
# script.  Each reproducer states its expected state, and the commit that
# closed it, in its own header.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# --list prints the expected-state lines of every reproducer and runs nothing.
if [ "${1:-}" = --list ]; then
  for t in "$HERE"/threads/*.lisp; do
    case "$(basename "$t")" in *prelude*) continue ;; esac
    echo "$(basename "$t" .lisp)"
    grep -E '^;+ *expected-state' "$t" | sed 's/^/    /' \
      || echo "    (no expected-state line: unknown)"
  done
  exit 0
fi
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
# -- the signal to flip the expected-state line in that test's header.
#
# The state is a property of the LISP UNDER TEST, not of the test file.  Run a
# `clean' row against a lisp that predates the commit it names and it will
# report UNEXPECTED FAILURE and fail this script -- correctly, because against
# that lisp the defect is present.
#
# Do not flip a state from a merge alone.  Every `clean' was measured on a
# kernel built from the commits it names.  One test that LOOKED ready to flip
# still reproduces, and only running it showed that.
#
# This is the XFAIL/XPASS pair every mature suite has (DejaGnu XPASS, lit XFAIL,
# pytest xfail(strict)).  RT's own *expected-failures* gives the XFAIL half but
# not the XPASS half, and it cannot help at all for a test that wedges the
# image, because a hung RT never reaches its report.
#
# WHERE THE STATE LIVES.  Each reproducer carries it in its own header, so a
# new test touches only its own file:
#
#     ;;; expected-state: clean
#     ;;; expected-state linuxx8664: repro
#
# The first line is the default.  A line that names the backend of the lisp
# under test (the PLATFORM below) overrides it.  The record of what was
# measured, and against which commits, sits next to that line.
#
# A file with no expected-state line has NO MEASURED STATE on this lisp.
# Reporting it `clean' hands an unmeasured test the best available outcome by
# default, which contradicts the rule above: the state is a property of the
# LISP UNDER TEST, and nobody has run this one here.  Measured 2026-09-17 --
# suspend-spinlock-static-cons.lisp arrived in this directory and the runner
# reported it `clean, as expected' before anyone had run it once.
PLATFORM="$("$CCL" --no-init --batch --quiet \
  -e '(progn (princ (string-downcase (ccl::backend-name ccl::*host-backend*))) (terpri) (quit))' \
  </dev/null 2>&1 | tail -1)"
echo "platform: $PLATFORM"
expected_state () {   # <file> -> repro | clean | unknown, for $PLATFORM
  local s=""
  [ -n "$PLATFORM" ] &&
    s="$(sed -nE "s/^;+ *expected-state $PLATFORM: *([a-z]+).*/\1/p" "$1" | head -1)"
  [ -n "$s" ] ||
    s="$(sed -nE 's/^;+ *expected-state: *([a-z]+).*/\1/p' "$1" | head -1)"
  case "$s" in
    repro|clean) echo "$s" ;;
    *)           echo unknown ;;
  esac
}
unexpected=0
for t in "$HERE"/threads/*.lisp; do
  case "$(basename "$t")" in *prelude*) continue ;; esac
  name="$(basename "$t" .lisp)"
  want="$(expected_state "$t")"
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
    echo "NO EXPECTED STATE -- it ran and reported '$got', but its header"
    echo "      has no expected-state line for $PLATFORM, so nothing here has"
    echo "      been measured against this lisp.  Add one naming the commit"
    echo "      that closes it, or 'repro' while the defect is open."
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
      echo "      set the expected-state line in its header to clean."
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
