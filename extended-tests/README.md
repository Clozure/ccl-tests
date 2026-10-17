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

Each one carries an **expected state** in its own header, a line such as
`;;; expected-state: clean`, with an optional per-platform line such as
`;;; expected-state linuxx8664: repro`. `run-all.sh --list` prints them all.
A reproducer for an open defect is expected to reproduce, and doing so is not
a failure. When it stops reproducing the runner says `UNEXPECTED-OK` and tells
you to flip its state — that is the `XPASS` half of the pair, and it is the
point of tracking state rather than simply skipping these tests. Because the
state lives in the test file, adding a reproducer changes no shared file.

That state describes the **lisp under test**, not the test file. A state of
`clean` means the defect behind it is fixed on master, so a reproduction is a
real failure that fails `run-all.sh`.
`sleep-vs-alloc` was the first `repro` state this directory had. Its fix,
`b3068522`, made it report UNEXPECTED-OK, which is the signal this machinery
exists to give, and its state was then flipped to `clean` from measured runs.

One test has already left this directory. `trylock-count-leak` did not wedge
the image, it was bounded by a 20-second wait, and its fix had landed, so it
needed none of the machinery here. It is now
`ccl.recursive-lock-trylock-count-leak` in `ansi-tests/ccl-stress.lsp`, an
ordinary `deftest` that `make test-stress` runs. A test belongs here only while
it needs an external timeout or its own process.

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

### threads/

**Three** of these track upstream issue **#597** and PR **#634**, which the
maintainer closed on 2026-09-17. The C-side fixes and the Lisp half are both
merged, so those three are green and `run-all.sh` expects each of them to
run **clean**. Run them against a lisp that predates the named commit and they
reproduce again, which is the point of naming the commit rather than a date.

**One does not belong to that family.** `sleep-vs-alloc.lisp` tracks issue
**#639**, fixed by `b3068522`, so it is expected to run clean. It is grouped here
because the class is threads and interrupts, not because it shares a cause with
the three above; nothing in the #597 work touches it.

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
One observation in three runs is not a state, so the state stays clean and a stall
now fails `run-all.sh`. It has not recurred: at pin `2b7422e6`, which carries
`2c382468`, the reproducer ran 500,000 of 500,000 with 285.8 million
allocations in 619 s. We never captured a backtrace of our own stall, so we
cannot claim `2c382468` explains it — only that the run is clean on a lisp that
carries it (med).

#### `windows-pipe-read.lisp` — Windows pipe reads, interrupts and GC

Windows only. It reads from `run-program` pipes while other threads interrupt
the reader, run a GC, or call `listen` on the same stream, and it checks end
of file and `run-program` output. Each child process is a second copy of the
same lisp, so the test needs only a CCL release.

The `interrupt-race` case reaches a kernel deadlock: `raise_thread_interrupt`
called `CancelSynchronousIo` on the target while the target was suspended in
`PeekNamedPipe`, inside `pipe_read`. The cancel waits for a thread that cannot
run. That stops the whole lisp, so no in-image timeout can report it; the
runner's external timeout does. Clozure/ccl PR #651 moves the cancel after
`ResumeThread`.

The case runs first, in 5 rounds of 100 interrupts, each round with a new
child and a new reader. Runs of the whole file, each under a 300 s timeout:

| kernel (Windows 11, x86-64, 1.13 image) | runs that hung |
|---|---|
| master `30f74b59` | 10 of 10 |
| master plus PR #651 | 0 of 10 (15 of 15 PASS each) |

#### `sleep-vs-alloc.lisp` — issue #639, fixed by `b3068522`

`(SLEEP n)` returns far too late while another thread allocates large objects.
Each GC suspends the sleeping thread with a signal, which interrupts
`#_nanosleep`; that returns EINTR, and `%nanosleep` then sleeps again for the
remaining time **the kernel reported**. The kernel computes that remainder
*before* it runs the handler, so the time the thread spends parked in
`suspend_resume_handler` — waiting for the collection to finish — is never
subtracted. Every world stop loses its own duration.

The error is therefore not a fixed offset. It grows with the collection RATE,
and the sleep converges only while it gains time faster than it loses it.

| cell | a 10 s sleep took | ratio | allocations |
|---|---|---|---|
| linuxx8664, `v1.13-465-g6526e21c`, t3.small | 31.0 s | 3.1× | 781,882 |
| linuxarm64, same pin, t4g.small | 168.0 s | 16.8× | 6,474,520 |
| linuxx8664, released 1.12.2 | about 50 s | 5× | — |

With `b3068522`, the same file returns on time, 3 of 3 on each architecture:

| cell | a 10 s sleep took | ratio | allocations |
|---|---|---|---|
| linuxx8664, `8d2b8d0e`, 2 vCPU | 10.0 s | 1.0× | 286,545 – 289,245 |
| linuxarm64, `8d2b8d0e`, 2 vCPU | 10.0 s | 1.0× | 374,783 – 381,698 |

⚠ **It is not established that the sleep never returns.** The unbounded
reading comes from runs that were KILLED (on an m9g.large, 3 of 3, at 90 s).
On the slower cells above the same sleep did return, after 3× and 17× its
requested time. What is measured is an overrun that grows with collection rate
and that nobody has found a bound for — not an infinite one. The file reports
the number it saw and does not decide that question.

⚠ **A throttled or low-core machine reproduces this MORE WEAKLY**, not more
strongly: fewer collections per second means less time lost per second. The
red cells above are burstable instances, so their numbers are conservative.

**The control that makes the red mean something.** The same file with one
variable changed — the allocation cut from 160,016 bytes to 176 — returns on
time on both architectures (10.6 s and 11.7 s) while allocating 30,297,490 and
27,336,462 times, which is 39× and 4× MORE allocation events than the runs
that fail. So the variable is object SIZE, and therefore how long a single
collection takes, rather than whether another thread allocates at all.

**The mechanism is not CCL-specific.** A small C program running the same
EINTR-and-re-sleep loop under a signal storm converges when its handler returns
at once, even at 124,553 interruptions, and diverges when the handler parks for
as little as 16 µs. The blocking handler is the variable, not the signal rate.

There is no table of the files here. Each file's header says what it
reproduces, which issue or PR it tracks, which widener it needs, and its
expected state; `bash run-all.sh --list` prints the states. A table that every
new reproducer appends a row to is a merge conflict between any two of them.

`arm64-red-prelude.lisp` and `arm64-widen-prelude.lisp` are loaded *by* the
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
5. A suite test: add a row to its directory's table in README.md, saying what
   it proves and whether it is green at the current tip. A reproducer in
   `threads/`: say what it proves in its header, and add its
   `;;; expected-state:` line there, `repro` while the defect is open.
6. If it needs a widener, put the patch in `wideners/` with a name that says
   what it widens, and say in README.md that it is test-only and must never
   merge.

## Keeping it consistent

Run this before proposing a change:

```sh
bash extended-tests/run-all.sh /path/to/ccl
```

Then check the three things a reviewer will check:

* **Every suite file in the tables, every reproducer with a state.** A test
  whose purpose nobody wrote down is a test nobody can maintain.
  `ls extended-tests/*/ *.lisp` against the tables, and
  `run-all.sh --list` for the reproducers.
* **Every expected-red test names its issue or PR.** When that merges, its
  state moves to clean and the file moves out of `threads/` if it no longer
  hangs.
* **No new absolute paths or line numbers.** `grep -rnE '/(local|home|tmp)/|\.(lisp|c|h|s):[0-9]+' extended-tests/`

## When a fix merges upstream

1. Re-run the test against the tip. It should now be green.
2. Move it out of `threads/` if it no longer hangs, into its class directory.
3. Set its state to clean and name the commit that fixed it: the
   expected-state line in its header for a reproducer, its README row for a
   suite test.
4. Delete the widener if nothing else uses it. A widener outliving its
   reproducer is a patch nobody can explain.
