EMACS ?= emacs
FORGE_LISP ?= ../forge/lisp
DEPS := closql compat cond-let emacsql ghub llama magit magit-section \
	markdown-mode seq transient treepy with-editor yaml
STRAIGHT_ROOT := $(lastword $(sort $(filter-out %.el,$(wildcard $(HOME)/.emacs.d/.local/straight/build-*))))
STRAIGHT_DIRS := $(foreach dep,$(DEPS),$(wildcard $(STRAIGHT_ROOT)/$(dep)))
ELPA_DIRS := $(foreach dep,$(DEPS),$(wildcard $(HOME)/.emacs.d/elpa/$(dep)-*))
LOAD_PATH := -L . -L $(FORGE_LISP) \
	$(foreach dir,$(STRAIGHT_DIRS),-L $(dir)) \
	$(foreach dir,$(ELPA_DIRS),-L $(dir))
BATCH := $(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq load-prefer-newer t)'

.PHONY: check compile test clean

check: clean compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile forge-dashboard.el test/forge-dashboard-test.el

test:
	$(BATCH) -l test/forge-dashboard-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f forge-dashboard.elc test/forge-dashboard-test.elc
