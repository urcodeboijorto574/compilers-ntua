#!/bin/bash

gcc -c lib.c -o lib.o
ar rcs lib.a lib.o
