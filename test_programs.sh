#!/bin/bash

# Declare counter of total, correct and erroneous programs
totalCounter=0
errorCounter=0
correctCounter=0

# Specify the directory
directory=$1

# Iterate through each file in the directory
for file in "$directory"*.grc
do
  if [ -f "$file" ]; then
    ((++totalCounter))
    # Extracting the file name without the directory path
    filename=$(basename "$file")

    # Remove any previously created executable
    if [ -f "./a.out" ]; then
      rm ./a.out
    fi

    # Print a start message
    echo "$filename:"

    # Run the file
    ./grace "$file"

    # If compilation was successful, run the executable
    if [ -f "./a.out" ]; then
      ((++correctCounter))
      ./a.out
    else
      ((++errorCounter))
    fi

    # Add a newline for better output separation
    echo
  fi
done

echo "Correct programs: $correctCounter/$totalCounter"
echo "Erroneous programs: $errorCounter/$totalCounter"
