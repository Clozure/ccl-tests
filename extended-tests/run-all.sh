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
# their result is reported but does NOT set the exit status.  See README.md for
# which fix each one waits on.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CCL="${1:-${CCL:-ccl}}"
TIMEOUT_SECS="${TIMEOUT_SECS:-180}"

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
# This is the XFAIL/XPASS pair every mature suite has (DejaGnu XPASS, lit XFAIL,
# pytest xfail(strict)).  RT's own *expected-failures* gives the XFAIL half but
# not the XPASS half, and it cannot help at all for a test that wedges the
# image, because a hung RT never reaches its report.
expected_state () {   # <name> -> repro | clean
  case "$1" in
    suspend-spinlock-deadlock) echo repro ;;   # upstream #597 / PR #634
    suspend-spinlock-rwlock)   echo repro ;;   # upstream #597 / PR #634
    unbind-missed-suspend)     echo repro ;;   # upstream #597 / PR #634
    trylock-count-leak)        echo repro ;;   # upstream #597 / PR #634
    *)                         echo clean ;;
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
  # the test's own non-zero verdict (trylock-count-leak exits 42 on a leak).
  case "$trc" in
    0)   got=clean ;;
    124) got=repro ;;
    *)   got=repro ;;
  esac
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
echo "=== run-all exit $rc (phase 1 only) ==="
exit $rc
