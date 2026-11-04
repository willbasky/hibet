module Type
    ( -- * Error
      HibetError (..)
    ) where

import Data.Text (Text)

data HibetError
    = EffectError Text Text
    | UnknownError Text
    deriving stock (Eq, Show)
