IMG ?= combobulate
TAG ?= dev
EMACS_VERSION ?= 29.4
JOBS ?= 4
DOCKER_CMD ?= docker run --rm -w /opt/ $(DOCKER_ARGS) $(IMG):$(TAG)
EMACS_BIN ?= emacs
ERT_JUNIT_FILE ?= $(CURDIR)/test-results/ert
EMACS_CMD = $(EMACS_BIN) --batch --no-init-file --chdir ./tests/ -L .. -L . -l .ts-test.el

.PHONY:	clean-elc
clean-elc:
	find . -name "*.elc" -delete

.PHONY:	byte-compile
byte-compile: clean-elc
	$(EMACS_BIN) \
	--no-init-file \
	--batch \
	--directory $(CURDIR) \
	--funcall batch-byte-compile \
	$(CURDIR) \
	*.el

.PHONY:	rebuild-relationships
rebuild-relationships:
	cd build ; \
	python build-relationships.py

.PHONY:	download-relationships
download-relationships:
	cd build ; \
	python build-relationships.py --download

.PHONY:	clean-tests
clean-tests: clean-elc
	find . -name "*.gen.el" -delete
	find . -name "*~" -delete
	find . -name "#*#" -delete
	rm -rf ./tests/fixture-deltas/* || true

.PHONY:	build-tests
build-tests: clean-tests
	$(EMACS_CMD) -l generate-harnesses.el

ELFILES := $(sort $(shell find ./tests/ -name "test-*.el" ! -name ".*" -print))
ERT_TEST_ARGS = $(patsubst %,-l %,$(ELFILES:.el=)) \
	--eval "(setq ert-summarize-tests-batch-and-exit nil)" \
	-f 'ert-run-tests-batch-and-exit'

.PHONY:	run-tests
run-tests: byte-compile
	$(EMACS_CMD) -l ert $(ERT_TEST_ARGS)

.PHONY:	run-tests-junit
run-tests-junit: byte-compile
	mkdir -p "$(dir $(ERT_JUNIT_FILE))"
	EMACS_TEST_JUNIT_REPORT="$(ERT_JUNIT_FILE)" \
	$(EMACS_CMD) -l ert \
	--eval '(setq ert-load-file-name "$(ERT_JUNIT_FILE)")' \
	$(ERT_TEST_ARGS)

.PHONY:	docker-build
docker-build:
	docker build \
		--build-arg EMACS_VERSION=$(EMACS_VERSION) \
		--build-arg JOBS=$(JOBS) \
		-t $(IMG):$(TAG) .

.PHONY:	docker-build-tests
docker-build-tests: docker-build
	$(DOCKER_CMD) build-tests

.PHONY:	docker-run-tests
docker-run-tests: docker-build
	$(DOCKER_CMD) run-tests
