module Utility
  (
    toText
  ) where

import qualified Data.Text as T
import Data.Text (Text)

toText :: Show a => a -> Text
toText = T.pack . show
