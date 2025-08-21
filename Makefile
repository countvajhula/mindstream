# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

SHELL=/bin/bash

PACKAGE-NAME=mindstream
DOCS-PATH=doc

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

EMACS=emacs
CASK ?= cask

PROJECT_FILES=`${CASK} files`

help:
	@echo "clean - remove all build artifacts"
	@echo "install - install package dependencies in .cask/"
	@echo "lint - check style with package-lint"
	@echo "lint+less - lint piped to less"
	@echo "lint-no-noise - lint with typically noisy warnings filtered out"
	@echo "lint-noiseless - lint-no-noise piped to less"
	@echo "checkdoc - check docstrings"
	@echo "build - byte compile the package"
	@echo "test - run tests"
	@echo "install-docs - Install dependencies for building the documentation"
	@echo "remove-docs - Uninstall docs package"
	@echo "build-docs - Build self-contained documentation that could be hosted somewhere"
	@echo "docs - view documentation in a browser"

install-docs:
	raco pkg install --deps search-auto -n $(PACKAGE-NAME) --link $(DOCS-PATH)

remove-docs:
	raco pkg remove $(PACKAGE-NAME)

build-docs:
	scribble ++style $(DOCS-PATH)/assets/css/$(PACKAGE-NAME).css --htmls --dest $(DOCS-PATH) --dest-name output $(DOCS-PATH)/mindstream.scrbl

build-pdf-docs:
	scribble --pdf --prefix $(DOCS-PATH)/pdf-style-prefix.tex --dest $(DOCS-PATH)/output $(DOCS-PATH)/mindstream.scrbl

docs: build-docs
	open $(DOCS-PATH)/output/index.html

check-docs-deps:
	raco setup --no-docs $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

clean :
	${CASK} clean-elc

install:
	${CASK} install

lint:
	${CASK} exec $(EMACS) -Q --batch  \
	                      -l "package-lint.el"  \
	                      --eval "(setq package-lint-main-file \"mindstream.el\")" \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}
checkdoc:
	${CASK} exec $(EMACS) -Q --batch  \
	                      -l "dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

build :
	${CASK} build

test: build
	${CASK} exec ert-runner

.PHONY:	help lint checkdoc build clean install test install-docs remove-docs build-docs docs
