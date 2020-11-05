{-# LANGUAGE TemplateHaskell #-}

module Assets
       (
         syllables
       , titles
       , dictinaries
       ) where

import Data.FileEmbed (embedFile, embedDir)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.ByteString (ByteString)


syllables :: Text
syllables = decodeUtf8 $(embedFile "stuff/tibetan-syllables")

titles :: Text
titles = decodeUtf8 $(embedFile "stuff/titles.toml")

dictinaries :: [(FilePath, ByteString)]
dictinaries = $(embedDir "dicts")
