import Test.Hspec

import Pasta
import Protolude hiding ((&))
import Lens.Micro

main :: IO ()
main = hspec $ do
  describe "selectFrom" $
    it "should build select command" $
      showt (selectFrom "some_table") `shouldBe`
      "SELECT * FROM \"some_table\" \"some_table\""
  describe "select" $ do
    it "should build select null command" $
      showt select `shouldBe`
      "SELECT NULL"
    it "should build select command using fromClause setter" $
      showt
      (select & columns .~ ("*" :| []) & fromClause .~ ["table1", "table2"])
      `shouldBe` "SELECT * FROM \"table1\" \"table1\", \"table2\" \"table2\""
    it "should build select command using fromClause and where setters" $
      showt
      (select & columns .~ ("*" :| []) & fromClause .~ ["table1"] & setWhere t)
      `shouldBe` "SELECT * FROM \"table1\" \"table1\" WHERE true"
    it "should build select command using NOT IN" $
      showt
      (select & columns .~ ("*" :| []) & fromClause .~ ["table1"] & setWhere (Not $ ("table1"//"c") `In` selectFrom "sub"))
      `shouldBe` "SELECT * FROM \"table1\" \"table1\" WHERE NOT \"table1\".\"c\" IN (SELECT * FROM \"sub\" \"sub\")"
  describe "selectFunction" $
    it "should build select version()" $
      showt (selectFunction ("public"//"version") []) `shouldBe` "SELECT \"public\".\"version\"()"
  describe "insert" $ do
    it "should build insert command" $
      showt (insert "foo" ("bar" :| []) ("qux" :| []))
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux')"
    it "should build insert command with on conflict" $
      showt (insert "foo" ("bar" :| []) ("qux" :| []) & onConflict .~ doNothing)
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux') ON CONFLICT DO NOTHING"
    it "should build insert command with on conflict update using literals" $
      showt (insert "foo" ("bar" :| []) ("qux" :| []) & onConflict .~ doUpdate "pkey" ["bar" .= "qux"])
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux') ON CONFLICT ON CONSTRAINT \"pkey\" DO UPDATE SET \"bar\" = 'qux'"
  describe "update" $ do
    it "should build update" $
      showt (update "foo" ("bar" :| []) ("qux" :| []))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux'"
    it "should build update with returning *" $
      showt (update "foo" ("bar" :| []) ("qux" :| []) & updateReturning .~ ["*"])
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' RETURNING *"
    it "should build update with condition" $
      showt (update "foo" ("bar" :| []) ("qux" :| []) & setUpdateFilter ("foo"//"bar" `eq` "baz"))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE \"foo\".\"bar\" = 'baz'"
{-
    it "should build update with function and operator" $
      showt (update "foo" ("bar" :| []) ("qux" :| []) & setWhere (LitExp "1 week"))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE age(ts) > '1 week'"
-}
