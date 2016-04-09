import Test.Hspec

import Pasta
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "selectFrom" $
    it "should build select command" $
      showt (selectFrom "some_table") `shouldBe` "SELECT * FROM \"some_table\" \"some_table\";"
  describe "select" $ do
    it "should build select null command" $
      showt select `shouldBe` "SELECT NULL;"
    it "should build select command using fromClause setter" $
      showt (select & columns .~ ("*" :| []) & fromClause .~ ["table1", "table2"]) `shouldBe` "SELECT * FROM \"table1\" \"table1\", \"table2\" \"table2\";"
    it "should build select command using fromClause and where setters" $
      showt (select & columns .~ ("*" :| []) & fromClause .~ ["table1"] & setWhere t) `shouldBe` "SELECT * FROM \"table1\" \"table1\" WHERE true;"
  describe "selectFunction" $
    it "should build select version()" $
      showt (selectFunction "version" []) `shouldBe` "SELECT \"version\"();"
