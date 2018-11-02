# cl-dependency-printer

Simple Common Lisp library to print dependency graph.

## Usage

Load cl-dependency-printer.lisp and evaluate the following macro:

    (print-dependency-graph &rest dep-groups)

An example usage:

    (print-dependency-graph
      ("main" "interp")
      ("interp" "yacc" "node" "runner" "env" "value" "garb-c")
      ("yacc" "lex" "node")
      ("lex" "yacc")
      ("node" "runner" "env" "value" "garb-c")
      ("runner" "env" "value" "garb-c")
      ("env" "garb-c")
      ("value" "garb-c")
      ("garb-c"))

Output:

    main   interp yacc   lex    node   runner env    value  garb-c
    |      |      |      |      |      |      |      |      |
    |----->|      |      |      |      |      |      |      |
    |      |      |      |      |      |      |      |      |
    |      |----->|      |      |      |      |      |      |
    |      |------|------|----->|      |      |      |      |
    |      |------|------|------|----->|      |      |      |
    |      |------|------|------|------|----->|      |      |
    |      |------|------|------|------|------|----->|      |
    |      |------|------|------|------|------|------|----->|
    |      |      |      |      |      |      |      |      |
    |      |      |----->|      |      |      |      |      |
    |      |      |------|----->|      |      |      |      |
    |      |      |      |      |      |      |      |      |
    |      |      |<-----|      |      |      |      |      |
    |      |      |      |      |      |      |      |      |
    |      |      |      |      |----->|      |      |      |
    |      |      |      |      |------|----->|      |      |
    |      |      |      |      |------|------|----->|      |
    |      |      |      |      |------|------|------|----->|
    |      |      |      |      |      |      |      |      |
    |      |      |      |      |      |----->|      |      |
    |      |      |      |      |      |------|----->|      |
    |      |      |      |      |      |------|------|----->|
    |      |      |      |      |      |      |      |      |
    |      |      |      |      |      |      |------|----->|
    |      |      |      |      |      |      |      |      |
    |      |      |      |      |      |      |      |----->|