module Pasta.Select 
    ( select
    , selectFrom
    , selectFunction
    , selectExp
    , columns
    , whereClause
    , relationAlias
    , relationExpression
    , exp
    , expression
    , alias
    , fromClause
    , setWhere
    , BooleanExpression (..)
    , Expression (..)
    , t
    , f
    ) where

import Pasta.Prelude
import qualified Data.Text as T

data BooleanExpression = Or (Expression, Expression)
                       | And (Expression, Expression)
                       | Not Expression
                       | BoolLiteral Bool
                       deriving (Eq, Show)

data Expression = IdentifierExp Identifier
                | BoolExp BooleanExpression
                | OperatorExp (Expression, Operator, Expression)
                | FunctionExp (Name, [Expression])
                | QueryExp Select
                | LiteralExp Literal
                | NameExp Name
                deriving (Eq, Show)

data Column = Column Expression
            | AliasedColumn
              { _expression :: Expression
              , _alias      :: Name
              } deriving (Eq, Show)

data FromRelation = FromRelation
              { _relationExpression :: Expression
              , _relationAlias      :: Name
              } deriving (Eq, Show)

data Select = Select
              { _columns     :: NonEmpty Column
              , _fromClause  :: [FromRelation]
              , _whereClause :: Maybe BooleanExpression
              } deriving (Eq, Show)

makeLenses ''Select
makeLenses ''FromRelation
makeLenses ''Column
makeLenses ''Expression

instance TextShow Expression where
  showb (IdentifierExp e) = showb e
  showb (BoolExp e) = showb e
  showb (OperatorExp (e1, Operator operator, e2)) = showb e1 <> " " <> fromText operator <> " " <> showb e2
  showb (FunctionExp (i, parameters)) = showb i <> "(" <> fromText (withCommas parameters) <> ")"
  showb (QueryExp e) = showb e
  showb (LiteralExp e) = showb e
  showb (NameExp e) = showb e

instance TextShow FromRelation where
  showb (FromRelation e a) = showb e <> " " <> showb a

instance TextShow Column where
  showb (Column c) = showb c
  showb (AliasedColumn c a) = showb c <> " AS " <> showb a

instance TextShow Select where
  showb (Select c fr w) =
    sel <> case w of
        Just ex -> " WHERE " <> showb ex
        Nothing -> ""
    where sel = "SELECT " <> fromText (neWithCommas c) <>
                   if null fr
                      then ""
                      else " FROM "
                    <> fromText (withCommas fr)

instance TextShow BooleanExpression where
  showb (Or (e1, e2)) = showb e1 <> " OR " <> showb e2
  showb (And (e1, e2)) = showb e1 <> " AND " <> showb e2
  showb (Not e) = "NOT " <> showb e
  showb (BoolLiteral True) = "true"
  showb (BoolLiteral False) = "false"

instance IsString Expression where
  fromString = LiteralExp . Literal . fromString

instance IsString Column where
  fromString = Column . NameExp . fromString

instance IsString FromRelation where
  fromString e = FromRelation (NameExp $ fromString e) (fromString e)

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
