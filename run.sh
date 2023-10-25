#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <input_file>"
    exit 1
fi

input_file="$1"

# Step 1: Run the first command
./grace < "$input_file"

# Step 2: Run the second command
llc -o a.s a.ll

# Step 3: Run the third command
clang -o a.out a.s ./lib/lib.a

# Step 4: Run the fourth command
./a.out