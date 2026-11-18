module Type
    ( -- * Error
      HibetError (..)
    , renderError
    ) where

import Control.Exception (Exception)
import Data.Text (Text)

data HibetError
    = EffectError Text Text
    | UnknownError Text
    deriving stock (Eq, Show)

instance Exception HibetError

-- | Human-readable error text: summary and detail, no constructor names.
renderError :: HibetError -> Text
renderError = \case
    EffectError summary detail -> summary <> "\n" <> detail
    UnknownError message -> message
