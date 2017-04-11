CCL=ccl

test:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e "(run-tests :exit t)"

clean:
	(cd ansi-tests && make clean)
