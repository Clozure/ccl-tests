# ccl-tests

This repository contains a modified version of Paul Dietz's Common Lisp
standards compliance test suite, as well as additional tests specific
to Clozure CL (http://github.com/Clozure/ccl).

To run the tests, you can use
```
make CCL=/path/to/ccl test
```
Alternatively, start up a ccl with `/path/to/dx86cl64 -n -l load.lisp` and then evaluate `(run-tests)`.
Replace `/path/to/dx86cl64` with the path to whatever ccl you want to run the tests in.
