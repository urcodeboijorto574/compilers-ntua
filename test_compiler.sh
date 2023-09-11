#!/bin/bash

COMPILER="./grace"

TEST_DIR="examples"
TEST_ERR_DIR="examples/erroneous"

echo "> Testing semantically correct programs:"
echo ""

# Loop through all test files in the directory
for test_file in "$TEST_DIR"/*.grc; do
	if [ -f "$test_file" ]; then
		echo "Testing $test_file..."

		$COMPILER < "$test_file"

		echo ""
		echo "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
		echo ""
	fi
done

echo "> Testing erroneous programs:"
echo ""

for test_file in "$TEST_ERR_DIR"/*.grc; do
	if [ -f "$test_file" ]; then
		echo "Testing $test_file..."

		$COMPILER < "$test_file"

		echo ""
		echo "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
		echo ""
	fi
done

