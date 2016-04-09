module Pasta.Types
    ( BooleanExpression (..)
    , Expression (..)
    , Select (..)
    , FromRelation (..)
    , Column (..)
    , Update (..)
    , Insert (..)
    , Name (..)
    , Identifier (..)
    , Literal (..)
    , Conflict (..)
    , ConflictTarget (..)
    , ConflictAction (..)
    , Assignment (..)
    ) where

import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import TextShow (TextShow, fromText, showb, showt)
import qualified Data.Text as T

-- Base types
newtype Operator = Operator T.Text deriving (Eq, Show)
newtype Literal = Literal T.Text deriving (Eq, Show)
newtype Name = Name T.Text deriving (Eq, Show)

data Identifier = Identifier
               { _qualifier  :: Name
               , _identifier :: Name
               } deriving (Eq, Show)

instance TextShow Literal where
  showb (Literal "NULL") = "NULL"
  showb (Literal e) = "$$" <> fromText e <> "$$"

instance IsString Literal where
  fromString = Literal . fromString

instance TextShow Name where
  showb (Name "*") = "*"
  showb (Name "EXCLUDED") = "EXCLUDED"
  showb (Name c) = showb c

instance IsString Name where
  fromString = Name . fromString

instance TextShow Identifier where
  showb (Identifier e1 e2) = showb e1 <> "." <> showb e2

-- Select Types
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
                | LitExp Literal
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

instance TextShow Expression where
  showb (IdentifierExp e) = showb e
  showb (BoolExp e) = showb e
  showb (OperatorExp (e1, Operator operator, e2)) = showb e1 <> " " <> fromText operator <> " " <> showb e2
  showb (FunctionExp (i, parameters)) = showb i <> "(" <> fromText (withCommas parameters) <> ")"
  showb (QueryExp e) = showb e
  showb (LitExp e) = showb e
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
    <> ";"
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
  fromString = LitExp . Literal . fromString

instance IsString Column where
  fromString = Column . NameExp . fromString

instance IsString FromRelation where
  fromString e = FromRelation (NameExp $ fromString e) (fromString e)

-- Update types
data Assignment = Assignment
                  { _targetColumn :: Name
                  , _assignmentValue :: Expression
                  } deriving (Eq, Show)

data Update = Update
              { _updateTarget       :: Identifier
              , _assignments  :: NonEmpty Assignment
              , _updateFilter :: Maybe BooleanExpression
              } deriving (Eq, Show)

instance TextShow Assignment where
  showb (Assignment e1 e2) = showb e1 <> " = " <> showb e2

-- Insert types
data ConflictTarget = OnConstraint Name
                      deriving (Eq, Show)

data Conflict = Conflict
                { _conflictTarget :: Maybe ConflictTarget
                , _conflictAction :: ConflictAction
                } deriving (Eq, Show)

data ConflictAction = DoNothing
                    | DoUpdate
                      { _conflictAssignments :: NonEmpty Assignment
                      , _conflictWhere :: Maybe BooleanExpression
                      } deriving (Eq, Show)

data Insert = Insert
              { _insertTarget :: Identifier
              , _insertColumns :: NonEmpty Name
              , _insertValues :: NonEmpty Expression
              , _onConflict :: Maybe Conflict
              } deriving (Eq, Show)

instance IsString ConflictTarget where
  fromString = OnConstraint . fromString

instance TextShow ConflictTarget where
  showb (OnConstraint e1) = fromText $ "ON CONSTRAINT " <> showt e1

instance TextShow Conflict where
  showb (Conflict Nothing e1) = fromText $ " ON CONFLICT " <> showt e1
  showb (Conflict (Just e1) e2) = fromText $ " ON CONFLICT " <> showt e1 <> " " <> showt e2

instance TextShow ConflictAction where
  showb DoNothing = "DO NOTHING"
  showb (DoUpdate e1 Nothing) = "DO UPDATE SET " <> fromText (neWithCommas e1)
  showb (DoUpdate e1 (Just e2)) = "DO UPDATE SET " <> fromText (neWithCommas e1) <> " WHERE " <> showb e2

instance TextShow Insert where
  showb (Insert e1 e2 e3 e4) =
    fromText $
    "INSERT INTO "
    <> showt e1
    <> " ("
    <> neWithCommas e2
    <> ") VALUES ("
    <> neWithCommas e3
    <> ")"
    <> fromMaybe "" (showt <$> e4)
    <> ";"

withCommas :: TextShow a => [a] -> T.Text
withCommas = T.intercalate ", " . map showt

neWithCommas :: TextShow a => NonEmpty a -> T.Text
neWithCommas = withCommas . toList
