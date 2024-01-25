#!/bin/bash

# Specify the directory
directory=$1

# Iterate through each file in the directory
for file in "$directory"*.grc
do
  if [ -f "$file" ]; then
    echo "Compiling and running $file:"
    immfile=${file::-3}imm
    asmfile=${file::-3}asm

    # Extracting the file name without the directory path
    filename=$(basename "$file")

    # Print a specific message before running the command
    ./grace -O "$file"

    # Run the file
    if [ -f "./a.out" ]; then
      ./a.out
    fi

    # Check if the files are generated
    if [ -f $immfile ]; then
      # Remove the unnecessary files (intermediate and final code)
      rm $immfile
    else
      echo "Error: Failed to generate .imm file for $filename"
    fi

    if [ -f $asmfile ]; then
      rm $asmfile
    else
      echo "Error: Failed to generate .asm file for $filename"
    fi

    # Add a newline for better output separation
    echo
  fi
done
