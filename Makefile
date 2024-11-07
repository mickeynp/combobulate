IMG ?= combobulate
TAG ?= dev
DOCKER_CMD ?= docker run --rm -w /opt/ $(DOCKER_ARGS) $(IMG):$(TAG)
EMACS_BIN ?= emacs
# Name of the directory where the Emacs setup is installed.
INIT_NAME := .emacs.d/
# Directory where the Emacs setup and TS installations take place.
TMP_INIT_DIR := $(PWD)/$(INIT_NAME)
# Baseline command for running tests.
EMACS_CMD = $(EMACS_BIN) --batch  --no-init-file --chdir ./tests/  -L ..  -L .  -l tests/.ts-setup.el

# Various installer scripts for testing installation
TEST_INSTALLERS := tests/.ts-loadpath.el tests/.ts-straight.el tests/.ts-use-package-vc.el

.PHONY:	clean-elc
clean-elc:
	find . -name "*.elc" -delete

.PHONY:	byte-compile
byte-compile: clean-elc
	$(EMACS_BIN) \
	--no-init-file \
	--batch \
	--directory $(PWD) \
	--funcall batch-byte-compile \
	$(PWD) \
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

.PHONY:	run-tests
run-tests: byte-compile
	$(EMACS_CMD) -l ert \
	$(patsubst %,-l %,$(ELFILES:.el=)) \
	--eval "(setq ert-summarize-tests-batch-and-exit nil)" \
	-f 'ert-run-tests-batch-and-exit'



# Clean test installers
.PHONY: clean-install-tests
clean-install-tests:
	rm -rf $(TMP_INIT_DIR)

.PHONY:	docker-build
docker-build:
	docker build -t $(IMG):$(TAG) .

.PHONY:	docker-build-tests
docker-build-tests: docker-build
	$(DOCKER_CMD) build-tests

.PHONY:	docker-run-tests
docker-run-tests: docker-build
	$(DOCKER_CMD) run-tests

clean: clean-elc clean-tests
	rm -rf $(TMP_INIT_DIR)


.PHONY: run-install-tests
run-install-tests: $(TEST_INSTALLERS)

$(TEST_INSTALLERS): byte-compile
	INIT_DIR=$(TMP_INIT_DIR)/$(basename $@); \
	$(EMACS_CMD) \
		--init-directory=$$INIT_DIR \
		-l tests/.ts-setup.el \
		-l $@ \
		-l tests/.ts-test.el
