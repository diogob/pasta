# Pasta [![Circle CI](https://circleci.com/gh/diogob/pasta/tree/master.svg?style=svg)](https://circleci.com/gh/diogob/pasta/tree/master)

Pasta is a very simple AST assembler for PostgreSQL's SQL dialect.
It provies a set of functions and lenses that will build queries, based on an internal AST representation.
It also provides instances for the [TextShow](https://github.com/RyanGlScott/text-show) class to convert
the generated AST into Text.

A simple select can be generated as:
```haskell
import Pasta

showt (selectFrom "some_table")
```

Note that `showt` (a function from TextShow module) is re-exported from Pasta for convenience. 
