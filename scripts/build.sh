#!/bin/sh

set -e

rm -f *.fasl
sbcl --script scripts/build.lisp
rm -f *.fasl

