{-# LANGUAGE TemplateHaskell #-}

module Spyglass
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
    , t
    , f
    , setWhere
    ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..), toList)
import           Data.Monoid        ((<>))
import           Data.String        (IsString, fromString)
import qualified Data.Text          as T
import           TextShow           (TextShow, fromText, showb, showt)

withCommas :: TextShow a => [a] -> T.Text
withCommas s = T.intercalate ", " $ fmap showt s

newtype Operator = Operator T.Text deriving (Eq, Show)
newtype Literal = Literal T.Text deriving (Eq, Show)
newtype Name = Name T.Text deriving (Eq, Show)

data Identifier = Identifier
               { _qualifier  :: Name
               , _identifier :: Name
               } deriving (Eq, Show)

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

data Assignment = Assignment
                  { _targetColumn :: Name
                  , _assignmentValue :: Expression
                  } deriving (Eq, Show)

data Update = Update
              { _updateTarget       :: Identifier
              , _assignments  :: NonEmpty Assignment
              , _updateFilter :: Maybe BooleanExpression
              } deriving (Eq, Show)

data Insert = Insert
              { _insertTarget       :: Identifier
              , _insertColumns  :: NonEmpty Name
              , _insertValues :: NonEmpty Expression
              } deriving (Eq, Show)

makeLenses ''Update
makeLenses ''Select
makeLenses ''FromRelation
makeLenses ''Column
makeLenses ''Expression

instance TextShow BooleanExpression where
  showb (Or (e1, e2)) = showb e1 <> " OR " <> showb e2
  showb (And (e1, e2)) = showb e1 <> " AND " <> showb e2
  showb (Not e) = "NOT " <> showb e
  showb (BoolLiteral True) = "true"
  showb (BoolLiteral False) = "false"

instance TextShow Identifier where
  showb (Identifier e1 e2) = showb e1 <> "." <> showb e2

instance TextShow Literal where
  showb (Literal e) = showb e

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

instance TextShow Name where
  showb (Name "*") = "*"
  showb (Name c) = showb c

instance TextShow Column where
  showb (Column c) = showb c
  showb (AliasedColumn c a) = showb c <> " AS " <> showb a

instance TextShow Select where
  showb (Select c fr w) =
    sel <> case w of
        Just ex -> " WHERE " <> showb ex
        Nothing -> ""
    where sel = "SELECT " <> fromText (withCommas $ toList c) <>
                   if null fr
                      then ""
                      else " FROM "
                    <> fromText (withCommas fr)

instance IsString Name where
  fromString = Name . fromString

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

t :: Expression
t = BoolExp $ BoolLiteral True

f :: Expression
f = BoolExp $ BoolLiteral False

setWhere :: BooleanExpression -> Select -> Select
setWhere = set whereClause . Just
