
EMACS_CMD = emacs \
	--chdir tests/ \
	--batch \
	--no-init-file \
	--eval '(setq load-prefer-newer t)' \
	--eval "(add-to-list 'load-path \"$(PWD)/tests/\")" \
	--eval "(add-to-list 'load-path \"$(PWD)\")" \
	--eval "(load \"combobulate-test-prelude\")" \
	-L ':.' \
	-L "$(PWD)/tests"

.PHONY:	byte-compile
byte-compile:
	@for file in *.el; do \
		rm -f $$filec; \
		emacs --no-init-file --eval '(setq load-prefer-newer t)' --directory $(PWD) --batch --funcall batch-byte-compile $$file; \
	done

.PHONY:	rebuild-relationships
rebuild-relationships:
	cd build ; \
	python build-relationships.py

.PHONY:	download-relationships
download-relationships:
	cd build ; \
	python build-relationships.py --download

.PHONY:	clear-tests
clear-tests:
	rm -vrf tests/fixture-deltas/
	rm -v tests/*.gen.el
.PHONY:	build-tests
build-tests:
	$(EMACS_CMD) -l tests/combobulate-generate-tests.el
	$(EMACS_CMD) -l tests/combobulate-generate-fixtures.el

ELFILES := $(sort $(shell find ${srcdir} tests/ -name "test-*.el" ! -name ".*" -print))

.PHONY:	run-tests
run-tests:
	$(EMACS_CMD) -l ert \
	$(patsubst %,-l %,$(ELFILES:.el=)) \
	--eval "(setq ert-summarize-tests-batch-and-exit nil)" \
	-f 'ert-run-tests-batch-and-exit'


