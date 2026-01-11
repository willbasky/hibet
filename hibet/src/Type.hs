module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)


data HibetError
  = EffectError Text Text
  | MegaError (ParseErrorBundle Text Void)
  | NotFound
  | NotSyllable Text
  | UnknownError Text
  deriving stock (Eq, Show)
