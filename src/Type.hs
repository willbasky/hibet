module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Path (PathException)
import Text.Megaparsec.Error (ParseErrorBundle)


data HibetError
  = PathError PathException
  | MegaError (ParseErrorBundle Text Void)
  | NotFound
  | NotSyllable Text
  | UnknownError Text
  deriving stock (Eq, Show)
