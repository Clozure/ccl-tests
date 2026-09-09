CCL=ccl

test:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e "(run-tests :exit t)"

test-ccl:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e "(run-tests :ansi nil :exit t)"

# The extended tests are NOT part of `test': several of them reproduce defects
# that are not fixed yet and are expected to fail until the fix lands.  See
# extended-tests/README.md.
test-extended:
	bash extended-tests/run-all.sh $(CCL)

clean:
	(cd ansi-tests && make clean)
