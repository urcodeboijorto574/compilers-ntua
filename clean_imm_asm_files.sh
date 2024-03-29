#!/bin/bash

# Iterate through each file in the directories given
for directory in "$@"
do
  if [ -n "$1" ]; then
    # Iterate through each file in the directory
    for file in "$directory"*.{imm,asm}
    do
      if [ -f "$file" ]; then
        # Remove the .imm/.asm file
        rm $file
      fi
    done
  else
    # Print usage message if argument is not given
    echo "Usage: $0 <directory-name>"
  fi
done
