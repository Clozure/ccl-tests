# ccl-tests

This repository contains a modified version of Paul Dietz's Common Lisp
standards compliance test suite, as well as additional tests specific
to Clozure CL (http://github.com/Clozure/ccl).

To run the tests, you can use
```
make CCL=/path/to/ccl test
```

Alternatively, start up a ccl with `/path/to/dx86cl64 -n -l load.lisp`
and then evaluate `(run-tests)`.  Replace `/path/to/dx86cl64` with the
path to whatever ccl you want to run the tests in.

The tests come in three files, all in `ansi-tests/`:

- `gclload2.lsp` loads the ANSI compliance tests.
- `ccl.lsp` contains CCL-specific tests.
- `ccl-stress.lsp` contains CCL-specific tests that take a while or
  use several threads.

`run-tests` loads all three by default.  Pass `:ansi nil`, `:ccl nil`,
or `:stress nil` to leave one out.  The make target `test-ccl` skips
the ANSI tests (so it runs `ccl.lsp` and `ccl-stress.lsp`), and
`test-stress` runs only the stress tests:
```
make CCL=/path/to/ccl test-ccl
make CCL=/path/to/ccl test-stress
```

A test in `ccl-stress.lsp` should not hang or damage the lisp
it runs in.  If it starts threads, it must wait for them with a
timeout, and clean up after itself.
