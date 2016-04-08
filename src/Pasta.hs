{-|
Module      : Pasta
Description : Assembles SQL statements
-}
module Pasta
    ( updateTarget
    , assignments
    , updateFilter
    , insert
    , insertTarget
    , insertColumns
    , insertValues
    , onConflict
    , select
    , selectExp
    , selectFrom
    , selectFunction
    , alias
    , expression
    , t
    , f
    , fromClause
    , relationAlias
    , relationExpression
    , setWhere
    , showt
    , NonEmpty (..)
    , fromList
    ) where

import Pasta.Types
import Control.Lens
import Data.List.NonEmpty (NonEmpty(..), fromList)
import TextShow (showt)
import qualified Data.Text as T

makeLenses ''Select
makeLenses ''FromRelation
makeLenses ''Column
makeLenses ''Expression
makeLenses ''Update
makeLenses ''Insert

select :: Select
select = Select ("*" :| []) [] Nothing

selectFrom :: Name -> Select
selectFrom table = select & fromClause .~ [FromRelation (NameExp table) table]

selectExp :: Expression -> Select
selectExp expr = select & columns .~ (Column expr :| [])

selectFunction :: T.Text -> [Expression] -> Select
selectFunction fn parameters = selectExp $ FunctionExp (Name fn, parameters)

setWhere :: BooleanExpression -> Select -> Select
setWhere = set whereClause . Just

t :: BooleanExpression
t = BoolLiteral True

f :: BooleanExpression
f = BoolLiteral False

-- | Builds an INSERT statement using a target, a non-empty list of column names and a non-empty list of values
insert :: T.Text -> NonEmpty T.Text -> NonEmpty T.Text -> Insert
insert target cols vals = Insert (Identifier schema table) colNames valExps Nothing
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
