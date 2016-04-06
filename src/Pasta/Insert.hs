module Pasta.Insert
       ( insert
       , insertTarget
       , insertColumns
       , insertValues
       ) where

import Pasta.Prelude
import Pasta.Select
import qualified Data.Text as T

data Insert = Insert
              { _insertTarget       :: Identifier
              , _insertColumns  :: NonEmpty Name
              , _insertValues :: NonEmpty Expression
              } deriving (Eq, Show)

makeLenses ''Insert

instance TextShow Insert where
  showb (Insert e1 e2 e3) = fromText $ "INSERT INTO " <> showt e1 <> " (" <> neWithCommas e2 <> ") VALUES (" <> neWithCommas e3 <> ")"

insert :: T.Text -> NonEmpty T.Text -> NonEmpty T.Text -> Insert
insert target cols vals = Insert (Identifier schema table) colNames valExps
  where
    qId = Name <$> T.split (=='.') target
    schema = if length qId == 2
               then head qId
               else "public"
    table = if length qId == 2
               then qId!!1
               else head qId
    colNames = Name <$> cols
    valExps = (LiteralExp . Literal) <$> vals
