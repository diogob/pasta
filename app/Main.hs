module Main where

import Pasta
import TextShow (printT)
import Control.Lens

main :: IO ()
main = do
  printT $ selectFrom "some_table"
  printT $ set fromClause ["another table"] (selectFrom "other")
  printT $ select & fromClause .~ ["table1", "table2"]
  printT $ select
    & fromClause .~ ["table1", "table2"]
    & setWhere t
  printT $ selectFunction "version" []
