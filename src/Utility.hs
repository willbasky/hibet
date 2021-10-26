module Utility
  (
    toText
  , filename
  , mkAbsolute
  , T.pack
  ) where

import Data.List.Extra (takeWhileEnd)
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory ( makeAbsolute )


toText :: Show a => a -> Text
toText = T.pack . show

filename :: FilePath -> FilePath
filename = takeWhileEnd (/= '/')

mkAbsolute :: FilePath -> FilePath
mkAbsolute = unsafePerformIO . makeAbsolute
