module Pasta.Prelude
       ( module Exports
       , Operator (..)
       , Literal (..)
       , Name (..)
       , Identifier (..)
       , withCommas
       , neWithCommas
       ) where

import Control.Lens as Exports
import Data.List.NonEmpty as Exports (NonEmpty(..), toList, fromList)
import Data.Monoid as Exports ((<>))
import Data.String as Exports (IsString, fromString)
import TextShow as Exports (TextShow, fromText, showb, showt)
import qualified Data.Text as T

newtype Operator = Operator T.Text deriving (Eq, Show)
newtype Literal = Literal T.Text deriving (Eq, Show)
newtype Name = Name T.Text deriving (Eq, Show)

data Identifier = Identifier
               { _qualifier  :: Name
               , _identifier :: Name
               } deriving (Eq, Show)

instance TextShow Literal where
  showb (Literal e) = "$$" <> fromText e <> "$$"

instance IsString Literal where
  fromString = Literal . fromString

instance TextShow Name where
  showb (Name "*") = "*"
  showb (Name c) = showb c

instance IsString Name where
  fromString = Name . fromString

instance TextShow Identifier where
  showb (Identifier e1 e2) = showb e1 <> "." <> showb e2

withCommas :: TextShow a => [a] -> T.Text
withCommas = T.intercalate ", " . map showt

neWithCommas :: TextShow a => NonEmpty a -> T.Text
neWithCommas = withCommas . toList
