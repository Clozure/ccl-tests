# extended-tests

Tests that are more complicated than the ANSI regression suite: reproducers for
defects found in the ports, kept in a form that can be read and re-run.

Nothing here is loaded by `make test`. These tests are **not** part of the green
bar, and several of them are *expected to fail* until a corresponding fix lands
— that is what they are for.

## Why a separate directory

The ANSI suite answers "does this implementation conform?". It cannot reach
whole classes of defect by construction, and every test below is one that
survived a full ANSI run of 21,679 tests with zero failures. The classes ANSI
cannot reach:

declared element types at `speed 3 / safety 0` (the open-coded array path) ·
FFI and callbacks · threads and interrupts · GC, purify and impurify ·
backtrace and debugger introspection · the disassembler · code coverage ·
fasl round-trips · `save-application` · printing of implementation objects.

The directories mirror those classes rather than the ports, so a **new port has
a checklist**: run `gc/` and `threads/` and you know what you have not yet
proven. A defect's file lives with its class, not with the architecture that
happened to expose it.

## Running them

```sh
bash extended-tests/run-all.sh /path/to/ccl      # everything
make test-extended CCL=/path/to/ccl              # the same, via the Makefile
```

Or from a REPL, for the suite half only:

```lisp
(load "extended-tests/load.lisp")
(run-extended-tests)
```

Every file finds its siblings through `*LOAD-TRUENAME*`, so this works from any
checkout. There is no installer, no environment variable and nothing to
configure.

## Two kinds of test, and they run differently

**The suite** (`backtrace/`, `compiler/`, `gc/`, `ffi/`,
`calling-convention/`) loads into one image and asserts. Each file prints
`XT-RESULT <name> TOTAL n FAILED n`. A failure is a regression.

**The reproducers** (`threads/`) each run in their **own process under an
external timeout**, and a timeout *is* the reproduction. This is not
belt-and-braces: a deadlock in the suspend/resume path stops every other thread
in the image, so an in-image watchdog is stopped too and can never fire. Only a
process-level timeout can observe it.

Each one carries an **expected state** in `run-all.sh`. A reproducer for an open
defect is expected to reproduce, and doing so is not a failure. When it stops
reproducing the runner says `UNEXPECTED-OK` and tells you to move its row to
green — that is the `XPASS` half of the pair, and it is the point of tracking
state rather than simply skipping these tests.

That state describes the **lisp under test**, not the test file. Every entry is
currently `clean`, because every defect in `threads/` is fixed on master. A
reproduction is therefore a real failure now, and it fails `run-all.sh`.

`trylock-count-leak.lisp` is the one that does not wedge: it is bounded by a
20-second wait and cleans up after itself. Its fix has now landed, so it is the
natural first candidate to become an ordinary `deftest` in the main suite, with
no `*expected-failures*` entry needed.

## What each test does

### backtrace/

| file | what it proves | green at tip? |
|---|---|---|
| `nvr-homed-local-recovery.lisp` | the debugger can read a local the compiler homed in a callee-saved register, and can set it. Broke when the arm64 NVR pool was populated while the backtrace side still carried empty-pool stubs. | yes — fixed by `104d1365` |
| `lexpr-frame-locals.lisp` | locals of a `&rest`/lexpr frame decode in backtrace instead of faulting on a bogus machine address. | yes — fixed by `2ccb2a20` |

### compiler/

| file | what it proves | green at tip? |
|---|---|---|
| `negative-index-bound-check.lisp` | a negative index is rejected by `aref`/`uvref` on the paths that do **not** open-code, not only by `svref`. The bound check must be unsigned. | yes — fixed by `72c714b3` |

### threads/  — all four are GREEN at tip

All four track upstream issue **#597** and PR **#634**, which the maintainer
closed on 2026-09-17. The C-side fixes and the Lisp half are both merged, so
every row below is green and `run-all.sh` now expects each of them to run
**clean**. Run them against a lisp that predates the named commit and they
reproduce again, which is the point of naming the commit rather than a date.

The Lisp half of #634 is **closed**. `*kernel-exception-lock*` and
`*kernel-tcr-area-lock*` name the same memory as the C structures and Lisp
reaches them, so suspend-awareness had to become a property of a particular
lock. The maintainer added `%lock-recursive-lock-ptr-deferring-suspension` and
its unlock counterpart in `2c382468`, then changed `with-exception-lock` to
expand into them. `suspend-spinlock-static-cons.lisp` covers that half: it
takes the same `RECURSIVE_LOCK` **from Lisp**, which is the acquire path no
other file here exercises.

⚠ **One unexplained stall, recorded because it is not reproducible.** On
linuxarm64 with 24 workers on 2 cores, one run of `suspend-spinlock-deadlock`
stopped emitting heartbeats at 75,750 of 500,000 iterations and then sat with 24
live threads for nine minutes. The stall began 73 seconds before the harness
signalled the process, so the signal did not cause it. A later run on the same
kernel and image completed all 500,000 with 282 million allocations, and the
maintainer measured 500,000 clean on darwinarm64, linuxarm64 and darwinx8664.
One observation in three runs is not a state, so the row stays green and a stall
now fails `run-all.sh`. It has not recurred: at pin `2b7422e6`, which carries
`2c382468`, the reproducer ran 500,000 of 500,000 with 285.8 million
allocations in 619 s. We never captured a backtrace of our own stall, so we
cannot claim `2c382468` explains it — only that the run is clean on a lisp that
carries it (med).

| file | what it reproduces | green at tip? | needs a widener? |
|---|---|---|---|
| `suspend-spinlock-deadlock.lisp` | a thread suspended by a world-stop while holding a lock-guard spin word never releases it; the world-stopper then spins forever taking that same word. Workers cons so allocation traps keep crossing the exception lock. | yes — fixed by `04f1e0ac` and `088e706e` | yes — `wideners/widen-guard-spinlock-window.patch` |
| `suspend-spinlock-static-cons.lisp` | the same `RECURSIVE_LOCK` as the row above, reached **from Lisp** rather than from an allocation trap. `static-cons` takes `*kernel-exception-lock*` on every call, so a worker can hold the guard word when the world stops. `suspend-spinlock-deadlock.lisp` reaches that lock through the C acquire path only, so until this file nothing here covered the other half of the defect. | yes — fixed by `2c382468` | **no** — the reds were measured unwidened; `wideners/widen-lisp-spin-release.patch` is optional |
| `trylock-count-leak.lisp` | `recursive_lock_trylock` raises the recursion count on the already-owned path and *then* returns EBUSY, so the caller releases once for its one acquisition and the lock stays owned forever. Reached through the kernel-import vector, the idiom level-0 already uses. | yes — fixed by `b5a00d12` | **no** — runs on a stock build |
| `unbind-missed-suspend.lisp` | `unbind_interrupt_level` reads the pending-suspend flag *before* restoring `*INTERRUPT-LEVEL*`, so a signal landing in between is deferred against the old level. The observable differs by architecture. On arm64 and x86-64 it is an ACK-latency spike. On 32-bit ARM it is a permanent wedge, because the forced-suspend block there dereferenced a register the entry path never loaded: a worker takes SIGSEGV inside the subprimitive and then deadlocks on the exception lock while the suspending thread waits for its acknowledgement. | yes — fixed by `1606a83d`, and `53a509a6` for 32-bit ARM | yes — the four `sled-*` patches, one pair per architecture |

`arm64-red-prelude.lisp` and `arm64-widen-prelude.lisp` are loaded *by* those
reproducers on arm64; they are not tests and `run-all.sh` skips them.

## wideners/

Test-only patches that widen a race window so it reproduces reliably. **None of
them is a fix and none should ever be merged.** They exist because a race that
reproduces once an hour is not a test.

The `sled-*` patches come in one pair per architecture, pre-fix and post-fix.
The pre-fix sled holds open the window between the flags read and the
`*INTERRUPT-LEVEL*` restore; the post-fix sled holds open the corresponding
window in the fixed order, with the same delay, so the two runs compare fairly.
The pair for one architecture is mutually exclusive: apply one or the other,
never both.

`trylock-count-leak.lisp` needs none of them — it is an external-observable
lock-count check, so it is the one to try first on an unmodified tree.

Apply with `git apply`, run the reproducer, then revert.

## Contributing a test

How to add a test here and keep the directory consistent. The rules are short
and each one exists because ignoring it produced a specific wrong result.

## The five rules

### 1. A test must pass on an implementation that has the fix

A test for a defect that is already fixed upstream belongs in the suite and must
be **green**. A test for a defect that is *not* yet fixed belongs in a directory
the suite does not load — today that is `threads/` — and must be listed in
README.md as expected-red, naming the issue or PR it waits on.

The failure this prevents: a permanently-red test in the default path teaches
everyone to ignore the bar.

### 2. Watch it fail before you trust it

A test that has never been seen to fail is not a test; it is a line of code that
returns true. Before adding one, produce the RED: revert the fix, or apply the
matching widener, and confirm the test reports the defect. Record what you
watched in the file's header — which lisp, and what it printed.

The failure this prevents: a green result that would have been green anyway.

### 3. No paths, no line numbers, no private infrastructure

A test must run from a plain checkout. That means:

* find siblings with `*LOAD-TRUENAME*` or `*LOAD-PATHNAME*`, never an absolute
  path and never an environment variable;
* no reference to a build directory, a lane, a CI job or a bug tracker that is
  not this repository's;
* **no `file.lisp:1234` line-number citations in comments.** They are wrong the
  moment anything above them moves. Say what the code does and why; name a
  commit if you need a pointer, since a commit hash does not rot.

### 4. State the class, not the architecture

A file lives in the directory for the defect class — `gc/`, `threads/`,
`backtrace/`, `compiler/`, `ffi/`, `calling-convention/` — not the port that
exposed it. Most of these defects are portable even when they were found on one
target, and a porter needs to run a class, not a history.

If a test genuinely is architecture-specific, guard it with a feature
conditional and say so in its header. Do not create an architecture directory.

### 5. A hang gets an external timeout, never an in-image watchdog

If the defect can stop the world, an in-image watchdog is stopped with
everything else and can never fire. Put such a test in `threads/`, make it
runnable as its own process, and let `run-all.sh` bound it with `timeout`. A
timeout is then a result, not a hung suite.

## Adding a test, step by step

1. Pick the directory by class (rule 4).
2. Write it against `harness.lisp`: `xt-check`, `xt-true`, `xt-skip`, then
   `(xt-report "NAME")`. Call `xt-report` with no keyword — the suite loader
   binds `*report-quits*` to nil so many files can run in one image, and a file
   run on its own still exits with a status.
3. Load the harness relatively:
   `(load (merge-pathnames "../harness.lisp" *load-pathname*))`
4. Watch it red (rule 2), then green.
5. Add a row to the table in README.md: what it proves, and whether it is green
   at the current tip.
6. If it needs a widener, put the patch in `wideners/` with a name that says
   what it widens, and say in README.md that it is test-only and must never
   merge.

## Keeping it consistent

Run this before proposing a change:

```sh
bash extended-tests/run-all.sh /path/to/ccl
```

Then check the three things a reviewer will check:

* **Every file in the tables.** A test not named in README.md is a test nobody
  knows the purpose of. `ls extended-tests/*/ *.lisp` against the tables.
* **Every expected-red test names its issue or PR.** When that merges, the row
  moves to green and the file moves out of `threads/` if it no longer hangs.
* **No new absolute paths or line numbers.** `grep -rnE '/(local|home|tmp)/|\.(lisp|c|h|s):[0-9]+' extended-tests/`

## When a fix merges upstream

1. Re-run the test against the tip. It should now be green.
2. Move it out of `threads/` if it no longer hangs, into its class directory.
3. Update its README row: green at tip, and name the commit that fixed it.
4. Delete the widener if nothing else uses it. A widener outliving its
   reproducer is a patch nobody can explain.
