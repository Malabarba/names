#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l dev/ert.el -l dev/examples-to-tests.el -l s.elc -l dev/examples.el -f ert-run-tests-batch-and-exit
