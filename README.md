# Grace Compiler

## Overview

Implementing a compiler for the programming language Grace, during the lesson "Compilers" in the 4th year of our studies in the school of Electrical and Computer Engineering, National Technical University of Athens.

## Features

### Grace Programming Language

- Short-circuiting: When the result of a logical operation can be evaluated just by the first argument, the second argument is not evaluated.

- Function overloading: Function overloading cannot happen in the range of one scope, but two functions can share the same name and/or signature if they are defined in different scopes.

- Function declarations and definitions: A function declaration must always be followed by its corresponding function definition. The parameters of a function's declaration and definition must match types and can not match names.

For more information about the technicalities of the Grace programming language, see the grace2023.pdf file.

### Grace compiler

- Take grace program from standard input and print the intermediate or the final code to standard output.

- Perform optimizations on the intermediate and the final code.

- Provide informative error messages due to lexical, syntactic or semantic errors.

## Getting started

### Prerequisites

The packages needed to build the compiler are the ones below:

- opam/2.1.4
- ocamlfind/1.9.5
- menhir/20231231
- menhirLib/20231231
- llvm/14.0.6
- clang
- gcc

### Installation

```bash
# Build compiler
make depend
make

# Remove object files (unnecessary files)
make clean

# Same as 'make clean', but also removes the executable program (./grace)
make distclean
```

## Usage

```bash
./grace [-O] [-i | -f] filename
    -O Optimizaztion flag
    -f Read from stdin, assembly code to stdout
    -i Read from stdin, intermediate code to stdout
    -help Display this list of options
    --help Display this list of options
```

Note: If neither of _-i_ and _-f_ flags are set, the compiler creates in the directory of the _.grc_ file a _.imm_ and a _.asm_ file, which contain the intermediate and the final code, respectively.

## Examples

To compile a simple hello-world program, run:

```bash
./grace examples/helloworld.grc
```

To run your program execute:

```bash
./a.out
```

## Contributing

Before opening a pull request ensure your code is properly formatted. To format your code we recommend the `ocamlformat` tool, version 0.26.1. To install it execute:

```bash
opam install ocamlformat.0.26.1
```

The formatting style is given in the `.ocamlformat` file in the base directory of the project. To format all (human generated) _.ml_ files execute:

```bash
make format
```

Alternatively, you can use your own formatting tool. Make sure the formatting rules are the same ones included in `.ocamlformat`.

<sub>Note: By default all files are gitignored in `.gitignore` file. To add your files exclude them names from `.gitignore`.</sub>

## Thanks!

If you found this project useful, we would be grateful if you could give it a star ⭐ on GitHub. Your support is appreciated!

## Acknowledgments

- [llvm.org](https://llvm.org)
- [llvm.moe](https://llvm.moe)

## Authors

- Dimitrios Vassiliou
- Ioannis Giannoukos
