#!/bin/bash

# Execute 'make format'
make format

# Execute 'make clean'
make clean

# Check the exit status of 'make clean'
if [ $? -eq 0 ]; then
  # 'make clean' was successful, so proceed with 'make depend'
  make depend

  # Check the exit status of 'make depend'
  if [ $? -eq 0 ]; then
    # 'make depend' was successful, so proceed with 'make'
    make
  else
    echo "Error: 'make depend' failed."
  fi
else
  echo "Error: 'make clean' failed."
fi
