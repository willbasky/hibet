module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Polysemy.Path (PathException)
import Text.Megaparsec.Error (ParseErrorBundle)


data HibetError
  = PathError PathException
  | MegaError (ParseErrorBundle Text Void)
  | BimapError Text
  | UnknownError Text
  deriving stock (Eq, Show)
