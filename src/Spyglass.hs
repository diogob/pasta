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
    , qualifier
    , expression
    , identifier
    , alias
    , fromClause
    , t
    , f
    , setWhere
    ) where

import Control.Lens
import Data.String (IsString, fromString)
import qualified Data.Text as T
import TextShow (TextShow, showt, showb, fromText)
import Data.Monoid ((<>))

withCommas :: (TextShow a) => [a] -> T.Text
withCommas showables = T.intercalate ", " $ map showt showables

type Name = T.Text
type Operator = T.Text

data Expression = Identifier Name
                | QualifiedIdentifier
                  { _qualifier :: Name
                  , _identifier :: Name
                  }
                | Boolean Bool
                | Or (Expression, Expression)
                | And (Expression, Expression)
                | Not Expression
                | BinaryOperator (Expression, Operator, Expression)
                | FunctionCall (Name, [Expression])
                  deriving (Eq)

data Column = Column
              { _expression :: Expression
              , _alias :: Name
              }
            | ColumnExpression Expression

data FromItem = Relation
              { _relationExpression :: Expression
              , _relationAlias :: Name
              }
            | RelationExpression Expression

data Select = Select
              { _columns :: [Column]
              , _fromClause :: [FromItem]
              , _whereClause :: Maybe Expression
              }

makeLenses ''Select
makeLenses ''FromItem
makeLenses ''Column
makeLenses ''Expression

instance TextShow Expression where
  showb (Boolean True) = "true"
  showb (Boolean False) = "false"
  showb (Identifier "*") = "*"
  showb (Identifier i) = showb i
  showb (QualifiedIdentifier q i) = showb q <> "." <> showb i
  showb (Or (e1, e2)) = showb e1 <> " OR " <> showb e2
  showb (And (e1, e2)) = showb e1 <> " AND " <> showb e2
  showb (Not e) = "NOT " <> showb e
  showb (BinaryOperator (e1, operator, e2)) = showb e1 <> " " <> fromText operator <> " " <> showb e2
  showb (FunctionCall (i, parameters)) = showb i <> "(" <> fromText (withCommas parameters) <> ")"

instance TextShow FromItem where
  showb (Relation c a) = showb c
                      <> " AS "
                      <> showb a
  showb (RelationExpression e) = showb e

instance TextShow Column where
  showb (Column c a) = showb c
                      <> " AS "
                      <> showb a
  showb (ColumnExpression e) = showb e

instance TextShow Select where
  showb (Select c f w) =
    select <> case w of
        Just ex -> " WHERE " <> showb ex
        Nothing -> ""
    where select = "SELECT " <> fromText (withCommas c) <>
                   if null f
                      then ""
                      else " FROM "
                    <> fromText (withCommas f)

instance IsString Expression where
  fromString = Identifier . fromString

instance IsString FromItem where
  fromString = RelationExpression . fromString

instance IsString Column where
  fromString = ColumnExpression . fromString

select :: Select
select = Select ["*"] [] Nothing

selectFrom :: Expression -> Select
selectFrom table = select & fromClause .~ [RelationExpression table]

selectExp :: Expression -> Select
selectExp expr = select & columns .~ [ColumnExpression expr]

selectFunction :: T.Text -> [Expression] -> Select
selectFunction fn parameters = selectExp $ FunctionCall (fn, parameters)

t :: Expression
t = Boolean True

f :: Expression
f = Boolean False

setWhere :: Expression -> Select -> Select
setWhere = set whereClause . Just
