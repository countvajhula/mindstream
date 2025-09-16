SHELL=/bin/bash

PACKAGE-NAME=mindstream
DOCS-PATH=doc

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

EMACS=emacs

export CI_PACKAGES=$(PACKAGE-NAME)

help:
	@echo "Run common development actions."
	@echo
	@echo "    Usage: make <target>"
	@echo "    where <target> is one of:"
	@echo
	@echo "help - show this menu"
	@echo "clean - remove all build artifacts"
	@echo "setup-ci - clone emacs-ci to run project CI actions such as linting"
	@echo "bootstrap - install Straight.el"
	@echo "install - install package dependencies"
	@echo "build - byte compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"
	@echo "install-docs - Install dependencies for building the documentation"
	@echo "remove-docs - Uninstall docs package"
	@echo "build-docs - Build self-contained documentation that could be hosted somewhere"
	@echo "build-pdf-docs - view documentation in a browser"
	@echo "docs - view documentation in a browser"
	@echo "check-docs-deps - view documentation in a browser"
	@echo
	@echo "**All of these actions (aside from docs) take effect and are contained inside the emacs-ci/ folder --- they do not affect the system Emacs configuration.**"

setup-ci:
	@if [ -d ".emacs-ci" ]; then \
		echo "--> Updating existing emacs-ci repository..."; \
		cd .emacs-ci && git pull; \
	else \
		echo "--> Cloning emacs-ci repository..."; \
		git clone https://github.com/countvajhula/emacs-ci.git .emacs-ci; \
	fi

clean:
	cd .emacs-ci && rm -rf ci-init

bootstrap:
	cd .emacs-ci && emacs --batch --quick --load bootstrap.el

install:
	cd .emacs-ci && emacs --batch --quick --load install.el

build:
	cd .emacs-ci && emacs --batch --quick --load build.el

lint:
	cd .emacs-ci && emacs --batch --quick --load lint.el

checkdoc:
	cd .emacs-ci && emacs --batch --quick --load checkdoc.el

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

.PHONY: help setup-ci clean bootstrap install build lint checkdoc install-docs remove-docs build-docs build-pdf-docs docs check-docs-deps
