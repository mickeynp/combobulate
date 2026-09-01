#!/bin/bash
# Script to run OxCaml-specific tests for Combobulate
# Usage: ./run-oxcaml-test.sh [options] [test-name-pattern]
#
# Options:
#   --all            Run all OxCaml tests (default)
#   --tag TAG        Run tests with specific tag (e.g., oxcaml, navigation)
#   --verbose        Show verbose output
#
# Examples:
#   ./run-oxcaml-test.sh                              # Run all tests
#   ./run-oxcaml-test.sh --tag oxcaml                 # Run tests tagged 'oxcaml'
#   ./run-oxcaml-test.sh oxcaml-1                     # Run specific test matching regex 'oxcaml-1'

set -e

cd "$(dirname "$0")"

# Default options
VERBOSE=""
SELECTOR=""

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --all)
            shift
            ;;
        --tag)
            SELECTOR="(quote (tag $2))"
            shift 2
            ;;
        --verbose)
            VERBOSE="--eval '(setq ert-batch-backtrace-right-margin 200)'"
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            # Assume it's a test name pattern
            SELECTOR="\"$1\""
            shift
            ;;
    esac
done

# Build the command
BASE_CMD="emacs --batch --no-init-file --chdir ./tests/ -L .. -L . -l .ts-test.el -l ert -l test-oxcaml-implementation-navigation.el"

# Build final command
if [ -z "$SELECTOR" ]; then
    CMD="$BASE_CMD --eval '(let ((backtrace-on-error-noninteractive nil)) (ert-run-tests-batch-and-exit))'"
else
    CMD="$BASE_CMD --eval '(let ((backtrace-on-error-noninteractive nil)) (ert-run-tests-batch-and-exit $SELECTOR))'"
fi

# Add verbose flag if requested
if [ -n "$VERBOSE" ]; then
    CMD="$CMD $VERBOSE"
fi

# Execute
eval $CMD
