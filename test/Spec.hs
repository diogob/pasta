import Test.Hspec

import Pasta
import Protolude hiding ((&))
import Lens.Micro

main :: IO ()
main = hspec $ do
  describe "selectFrom" $
    it "should build select command" $
      toSQL (selectFrom "some_table") `shouldBe`
      "SELECT * FROM \"some_table\" \"some_table\" WHERE true"
  describe "select" $ do
    it "should build select null command" $
      toSQL select `shouldBe`
      "SELECT NULL WHERE true"
    it "should build select command using relations setter" $
      toSQL
      (select & columns .~ ("*" :| []) & relations .~ ["table1", "table2"])
      `shouldBe` "SELECT * FROM \"table1\" \"table1\", \"table2\" \"table2\" WHERE true"
    it "should build select command using relations and where setters" $
      toSQL
      (select & columns .~ ("*" :| []) & relations .~ ["table1"] & conditions .~ f)
      `shouldBe` "SELECT * FROM \"table1\" \"table1\" WHERE false"
    it "should build select command using NOT IN" $
      toSQL
      (select & columns .~ ("*" :| []) & relations .~ ["table1"] & conditions .~ (Not $ ("table1"//"c") `In` selectFrom "sub"))
      `shouldBe` "SELECT * FROM \"table1\" \"table1\" WHERE NOT \"table1\".\"c\" IN (SELECT * FROM \"sub\" \"sub\" WHERE true)"
  describe "selectFunction" $
    it "should build select version()" $
      toSQL (selectFunction ("public"//"version") []) `shouldBe` "SELECT \"public\".\"version\"() WHERE true"
  describe "insert" $ do
    it "should build insert command" $
      toSQL (insert "foo" ("bar" :| []) ("qux" :| []))
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux')"
    it "should build insert command with on conflict" $
      toSQL (insert "foo" ("bar" :| []) ("qux" :| []) & onConflict .~ doNothing)
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux') ON CONFLICT DO NOTHING"
    it "should build insert command with on conflict update using identifiers" $
      toSQL (insert "foo" ("bar" :| []) ("qux" :| []) & onConflict .~ doUpdate "pkey" ["bar" .= ("EXCLUDED"//"qux")])
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux') ON CONFLICT ON CONSTRAINT \"pkey\" DO UPDATE SET \"bar\" = EXCLUDED.\"qux\" WHERE true"
    it "should build insert command with on conflict update using literals" $
      toSQL (insert "foo" ("bar" :| []) ("qux" :| []) & onConflict .~ doUpdate "pkey" ["bar" .= ("qux" :: Text)])
      `shouldBe` "INSERT INTO \"public\".\"foo\" (\"bar\") VALUES ('qux') ON CONFLICT ON CONSTRAINT \"pkey\" DO UPDATE SET \"bar\" = 'qux' WHERE true"
  describe "update" $ do
    it "should build update" $
      toSQL (update "foo" ("bar" :| []) ("qux" :| []))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE true"
    it "should build update with returning *" $
      toSQL (update "foo" ("bar" :| []) ("qux" :| []) & returning .~ ["*"])
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE true RETURNING *"
    it "should build update with condition" $
      toSQL (update "foo" ("bar" :| []) ("qux" :| []) & conditions .~ ("foo"//"bar" `eq` ("baz" :: Text)))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE \"foo\".\"bar\" = 'baz'"
{-
    it "should build update with function and operator" $
      toSQL (update "foo" ("bar" :| []) ("qux" :| []) & setWhere (LitExp "1 week"))
      `shouldBe` "UPDATE \"public\".\"foo\" SET \"bar\" = 'qux' WHERE age(ts) > '1 week'"
-}
