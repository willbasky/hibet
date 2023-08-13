module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Path (PathException)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Exception (IOException)


data HibetError
  = PathError PathException
  | FileError IOException Text
  | MegaError (ParseErrorBundle Text Void)
  | NotFound
  | NotSyllable Text
  | UnknownError Text
  deriving stock (Eq, Show)
