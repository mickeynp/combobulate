IMG ?= combobulate
TAG ?= dev
DOCKER_CMD ?= docker run --rm -w /opt/ $(DOCKER_ARGS) $(IMG):$(TAG)
EMACS_CMD = emacs \
	--batch \
	--no-init-file \
	--eval '(princ default-directory)' \
	--eval '(setq load-prefer-newer t)' \
	--chdir ./tests/ \
	-L .. \
	-L .

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

.PHONY:	clean-tests
clean-tests:
	find . -name "*.gen.el" -delete
	find . -name "*.elc" -delete
	find . -name "*~" -delete
	find . -name "#*#" -delete
	rm -rf ./tests/fixture-deltas/* || true

.PHONY:	build-tests
build-tests: clean-tests
	$(EMACS_CMD) -l generate-harnesses.el

ELFILES := $(sort $(shell find ${srcdir} -name "test-*.el" ! -name ".*" -print))

.PHONY:	run-tests
run-tests:
	$(EMACS_CMD) -l ert \
	$(patsubst %,-l %,$(ELFILES:.el=)) \
	--eval "(setq ert-summarize-tests-batch-and-exit nil)" \
	-f 'ert-run-tests-batch-and-exit'

.PHONY:	docker-build
docker-build:
	docker build -t $(IMG):$(TAG) .

.PHONY:	docker-build-tests
docker-build-tests: docker-build
	$(DOCKER_CMD) build-tests

.PHONY:	docker-run-tests
docker-run-tests: docker-build
	$(DOCKER_CMD) run-tests
