module Type
    ( -- * Error
      HibetError (..)
    ) where

import Control.Exception (Exception)
import Data.Text (Text)

data HibetError
    = EffectError Text Text
    | UnknownError Text
    deriving stock (Eq, Show)

instance Exception HibetError
