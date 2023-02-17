.PHONY:	byte-compile
byte-compile:
	@for file in *.el; do \
		emacs --no-init-file --eval '(setq load-prefer-newer t)' --directory $(PWD) --batch --funcall batch-byte-compile $$file; \
	done

.PHONY:	rebuild-relationships
rebuild-relationships:
	cd build ; \
	python build-relationships.py

download-relationships:
	cd build ; \
	python build-relationships.py --download
