module Main where

import Spyglass (selectFrom)
import TextShow (printT)

main :: IO ()
main = printT $ selectFrom "some_table"
