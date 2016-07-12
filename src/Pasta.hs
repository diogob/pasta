{-|
Module      : Pasta
Description : Assembles SQL statements
-}
module Pasta
    ( updateTarget
    , assignments
    , updateFilter
    , selectFilter
    , insert
    , insertTarget
    , insertColumns
    , insertValues
    , update
    , updateReturning
    , onConflict
    , doNothing
    , doUpdate
    , (.=)
    , (//)
    , conflictTarget
    , conflictAction
    , conflictAssignments
    , conflictWhere
    , select
    , selectExp
    , selectFrom
    , selectFunction
    , alias
    , columns
    , expression
    , t
    , f
    , fromRelations
    , relationAlias
    , relationExpression
    , toSQL
    , NonEmpty (..)
    , fromList
    , Expression(Null)
    , BooleanExpression(Not, In)
    , (.|)
    , (.&)
    , (.!)
    , cmp
    , eq
    , gt
    , lt
    , gte
    , lte
    , fn
    , now
    , age
    ) where

import Protolude hiding ((&))
import Pasta.Types
import Lens.Micro
import Lens.Micro.TH
import Data.List.NonEmpty (NonEmpty(..), fromList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

makeLenses ''Select
makeLenses ''FromRelation
makeLenses ''Column
makeLenses ''Expression
makeLenses ''Update
makeLenses ''Insert
makeLenses ''Conflict
makeLenses ''ConflictAction

-- | Builds a BooleanExpression out of an operator and 2 expressions
cmp :: (IsExpression lexp, IsExpression rexp) => Text -> lexp -> rexp -> BooleanExpression
cmp op lexp rexp = Comparison (Operator op) (toExp lexp) (toExp rexp)

eq :: (IsExpression lexp, IsExpression rexp) => lexp -> rexp -> BooleanExpression
eq = cmp "="

gt :: (IsExpression lexp, IsExpression rexp) => lexp -> rexp -> BooleanExpression
gt = cmp ">"

lt :: (IsExpression lexp, IsExpression rexp) => lexp -> rexp -> BooleanExpression
lt = cmp "<"

gte :: (IsExpression lexp, IsExpression rexp) => lexp -> rexp -> BooleanExpression
gte = cmp ">="

lte :: (IsExpression lexp, IsExpression rexp) => lexp -> rexp -> BooleanExpression
lte = cmp "<="

-- | Builds a SELECT null with neither FROM nor WHERE clauses.
select :: Select
select = Select (Column Null :| []) [] t

-- | Builds a SELECT * FROM table statement.
selectFrom :: Name -> Select
selectFrom table = select & columns .~ ("*" :| []) & fromRelations .~ [FromRelation (NameExp table) table]

-- | Builds a SELECT expression with neither FROM nor WHERE clauses
selectExp :: Expression -> Select
selectExp expr = select & columns .~ (Column expr :| [])

-- | Builds a SELECT fn(parameters) with neither FROM nor WHERE clauses
selectFunction :: Identifier -> [Expression] -> Select
selectFunction fnId parameters = selectExp $ fn fnId parameters

-- | Builds a function
fn :: Identifier -> [Expression] -> Expression
fn = FunctionExp

-- | Builds a now() function
now :: Expression
now = fn ("pg_catalog"//"now") []

-- | Builds an age(t) function
age :: IsExpression exp => exp -> Expression
age time = fn ("pg_catalog"//"age") [toExp time]


-- | Just a convenient way to write a BoolLiteral True
t :: BooleanExpression
t = BoolLiteral True

-- | Just a convenient way to write a BoolLiteral False
f :: BooleanExpression
f = BoolLiteral False

-- | Builds an INSERT statement using a target, a non-empty list of column names and a non-empty list of values
insert :: T.Text -> NonEmpty T.Text -> NonEmpty T.Text -> Insert
insert target cols vals = Insert (Identifier schema table) colNames valExps Nothing
  where
    (schema, table) = splitTarget target
    colNames = Name <$> cols
    valExps = (LitExp . Literal) <$> vals

-- | Builds an UPDATE statement using a target, a non-empty list of column names and a non-empty list of values
update :: T.Text -> NonEmpty T.Text -> NonEmpty Expression -> Update
update target cols vals = Update (Identifier schema table) assigns t []
  where
    (schema, table) = splitTarget target
    assigns = NE.zipWith Assignment (Name <$> cols) vals

doNothing :: Maybe Conflict
doNothing = Just $ Conflict Nothing DoNothing

doUpdate :: ConflictTarget -> [Assignment] -> Maybe Conflict
doUpdate _ [] = Nothing
doUpdate target assigns =
  Just $
  Conflict (Just target) $
  DoUpdate (fromList assigns) t

(.=) :: IsExpression exp => Name -> exp -> Assignment
(.=) e ex = Assignment e $ toExp ex

(//) :: Name -> Name -> Identifier
(//) = Identifier

(.|) :: BooleanExpression -> BooleanExpression -> BooleanExpression
(.|) = Or

(.&) :: BooleanExpression -> BooleanExpression -> BooleanExpression
(.&) = And

infixr 0 .!
(.!) :: BooleanExpression -> BooleanExpression
(.!) = Not

-- private functions

splitTarget :: T.Text -> (Name, Name)
splitTarget target = (schema, table)
  where
    qId = Name <$> T.split (=='.') target
    schema = case qId of
              [s, _] -> s
              _ -> "public"
    table = case qId of
              [_, tbl] -> tbl
              [tbl] -> tbl
              _ -> ""
