#!/bin/bash
# This script is meant to be ran from lib/ directory.
gcc -c lib.c -o lib.o
ar rcs lib.a lib.o
