# PASTA [![Circle CI](https://circleci.com/gh/diogob/pasta/tree/master.svg?style=svg)](https://circleci.com/gh/diogob/pasta/tree/master)

PASTA stands for **PostgreSQL Abstract Syntax Tree Assembler**.
It provies a set of functions and lenses that will build queries, based on an internal AST representation.
It also provides instances for the [TextShow](https://github.com/RyanGlScott/text-show) class to convert
the generated AST into Text.

In other words, it allows you to:

 * Write SQL queries in Haskell without resorting to string/text concatenation.
 * Catch several SQL syntax errors as type-cheker errors.
 
It won't help you with:

 * Encoding or decoding values, connecting to the database or executing the queries, for this you can rely on database connection libraries such as:
   * [hasql](https://github.com/nikita-volkov/hasql)
   * [postgresql-simple](https://github.com/lpsmith/postgresql-simple)
   
 * Creating higher level abstractions over database objects, for this check:
   * [persistent](https://github.com/yesodweb/persistent)
   * [opaleye](https://github.com/tomjaguarpaw/haskell-opaleye)
   * [beam](https://github.com/tathougies/beam)

A simple select can be generated as:
```haskell
import Pasta

showt (selectFrom "some_table")
```

Note that `showt` (a function from [TextShow](http://hackage.haskell.org/package/text-show) module) is re-exported from Pasta for convenience. 

A ```INSERT``` statement with conflict resolution (aka UPSERT).
```haskell
import Pasta

showt $
    insert "public.foo" 
        ("bar" :| ["qux"]) 
        ("2" :| ["3"])
    & onConflict .~ doUpdate "foo_pkey" 
    ["bar" =.= ("EXCLUDED"//"bar"), "qux" =.= ("EXCLUDED"//"qux")]
```

Note that `:|` (an operator from [semigroups](http://hackage.haskell.org/package/semigroups) module) is re-exported from PASTA for convenience. The **NonEmpty** type is used in several PASTA functions.
