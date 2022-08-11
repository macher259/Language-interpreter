# Imperative language Interpreter

# Building and running:
To build it you need BNF Converter. You can download it using:
`stack install alex happy BNFC`

```
$ make
$ ./interpreter [FILE]
```
If `[FILE]` is not provided, the program will read from STDIN.

# Language grammar:
Grammar is described in [grammar.cf](grammar.cf).

# Language supports:
- variables, literals, basic arithmetic, comparison
- if and while
- runtime errors
- functions with recursion
- static typing
- static binding
- passing function argument by value and by reference

# Examples:
- `good` directory contains examples of valid programs
- `bad` directory contains examples of invalid programs e.g. no return statement in non-void function
