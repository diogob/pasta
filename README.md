# PASTA ![Haskell CI](https://github.com/diogob/pasta/workflows/Haskell%20CI/badge.svg)

PASTA stands for **PostgreSQL Abstract Syntax Tree Assembler**.
It provies a set of functions and lenses that will build queries, based on an internal AST representation.
It also provides a `toSQL` function the generated AST into Text.

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

toSQL (selectFrom "some_table")
```

Note that `toSQL` currently is implemented using `showt` (a function from [TextShow](http://hackage.haskell.org/package/text-show) module).

A ```INSERT``` statement with conflict resolution (aka UPSERT).
```haskell
import Pasta

toSQL $
    insert "public.foo" ("bar" :| ["qux"]) ("2" :| ["3"])
    & onConflict .~ doUpdate "foo_pkey" ["bar" .= ("qux" :: Text)]
```

The above `toSQL` will result in:
```sql
INSERT INTO "public"."foo" ("bar") VALUES ('qux') ON CONFLICT ON CONSTRAINT "pkey" DO UPDATE SET "bar" = 'qux' WHERE true
```

You can use the `//` operator to build fully qualified identifiers as in:

```haskell
toSQL
  ( select 
  & columns .~ ("*" :| []) 
  & relations .~ ["table1"] 
  & conditions .~ (("table1"//"c") `In` selectFrom "sub")
  )
```

Wich results in:
```sql
SELECT * FROM "table1" "table1" WHERE "table1"."c" IN (SELECT * FROM "sub" "sub" WHERE true)
```

Note that `:|` (an operator from [semigroups](http://hackage.haskell.org/package/semigroups) module) is re-exported from PASTA for convenience. The **NonEmpty** type is used in several PASTA functions.
