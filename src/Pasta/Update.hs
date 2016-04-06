module Pasta.Update
       ( updateTarget
       , assignments
       , updateFilter
       ) where

import Pasta.Prelude
import Pasta.Select

data Assignment = Assignment
                  { _targetColumn :: Name
                  , _assignmentValue :: Expression
                  } deriving (Eq, Show)

data Update = Update
              { _updateTarget       :: Identifier
              , _assignments  :: NonEmpty Assignment
              , _updateFilter :: Maybe BooleanExpression
              } deriving (Eq, Show)

makeLenses ''Update
