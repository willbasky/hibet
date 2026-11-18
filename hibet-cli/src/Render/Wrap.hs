-- | Line breaks for a Tibetan query: only at syllable boundaries, never
-- inside a stack.
module Render.Wrap
    ( syllables
    , wrapQuery
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The syllables of a query, each keeping its trailing tsek @་@. Text with
-- no tsek is one piece.
syllables :: Text -> [Text]
syllables text
    | T.null text = []
    | T.null rest = [syllable]
    | otherwise = (syllable <> "་") : syllables (T.drop 1 rest)
    where
        (syllable, rest) = T.breakOn "་" text

-- | Lines no wider than @width - 4@ code points. A syllable wider than that
-- keeps its own line and is not cut: the margin leaves room for the header
-- indent, and a code point is never narrower than the cell it draws in.
wrapQuery :: Int -> Text -> [Text]
wrapQuery width = go [] "" . syllables
    where
        limit = max 1 (width - 4)

        go done current [] = reverse (add current done)
        go done current (syllable : rest)
            | T.null current = go done syllable rest
            | T.length current + T.length syllable <= limit =
                go done (current <> syllable) rest
            | otherwise = go (add current done) syllable rest

        add "" done = done
        add current done = current : done
