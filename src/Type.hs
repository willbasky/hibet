module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import GHC.Stack ( CallStack )


data HibetError
  = EffectError Text CallStack
  | MegaError (ParseErrorBundle Text Void)
  | NotFound
  | NotSyllable Text
  | UnknownError Text
  deriving stock Show
