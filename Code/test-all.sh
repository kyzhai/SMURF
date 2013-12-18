#!/bin/bash
echo "$*\n"
echo "\033[1;4mParser Tests\033[0m" 1>&2
sh test-parser.sh

echo "$*\n"
echo "\033[1;4mSemantic Tests\033[0m" 1>&2
sh test-semantic.sh

echo "$*\n"
echo "\033[1;4mTop-Level Tests\033[0m" 1>&2
sh test-toplevel.sh
