#!/bin/bash

# This script is to automate running tests for my emacs configuration.

emacs -batch -l ert -l test_base-library.el -f ert-run-tests-batch-and-exit

# This is directing the output to a log file.
# emacs -batch -l ert -f ert-summarize-tests-batch-and-exit output.log

# This is how.
# The manual says some talk about trying to make sure that the print length is
# truncated.  I do not know.  I would rather have a print-length that is too long
# than one that is too short.

# I am putting this stuff into a file.
emacs -batch -l ert -l my-tests.el --eval "(let ((ert-batch-print-level 10) (ert-batch-print-length 120)) (ert-run-tests-batch-and-exit))"
