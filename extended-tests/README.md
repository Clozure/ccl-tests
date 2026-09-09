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

`trylock-count-leak.lisp` is the one that does not wedge: it is bounded by a
20-second wait and cleans up after itself, so it is also the natural first
candidate to become an ordinary `deftest` registered in RT's own
`*expected-failures*` once the fix lands.

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

### threads/  — expected RED until the fixes merge

All four track upstream issue **#597** and PR **#634**.

| file | what it reproduces | needs a widener? |
|---|---|---|
| `suspend-spinlock-deadlock.lisp` | a thread suspended by a world-stop while holding a lock-guard spin word never releases it; the world-stopper then spins forever taking that same word. Workers cons so allocation traps keep crossing the exception lock. | yes — `wideners/widen-guard-spinlock-window.patch` |
| `suspend-spinlock-rwlock.lisp` | the same defect via rwlock **read** locks only, which rules out the by-design "suspended lock owner" hazard: a reader never blocks a reader, so a wedge can only be the guard spin word. | yes — `wideners/widen-lisp-spin-release.patch` |
| `trylock-count-leak.lisp` | `recursive_lock_trylock` raises the recursion count on the already-owned path and *then* returns EBUSY, so the caller releases once for its one acquisition and the lock stays owned forever. Reached through the kernel-import vector, the idiom level-0 already uses. | **no** — runs on a stock build |
| `unbind-missed-suspend.lisp` | `unbind_interrupt_level` reads the pending-suspend flag *before* restoring `*INTERRUPT-LEVEL*`, so a signal landing in between is deferred against the old level. The observable is an ACK-latency spike, not a permanent wedge. | yes — the two `sled-*` patches |

`arm64-red-prelude.lisp` and `arm64-widen-prelude.lisp` are loaded *by* those
reproducers on arm64; they are not tests and `run-all.sh` skips them.

## wideners/

Test-only patches that widen a race window so it reproduces reliably. **None of
them is a fix and none should ever be merged.** They exist because a race that
reproduces once an hour is not a test.

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
