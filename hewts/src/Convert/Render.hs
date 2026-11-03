-- | The output arm of the converter: which script the result is written in,
-- and the one entry point that dispatches to it. The two arms walk the items
-- differently: Unicode prints one item at a time, Wylie walks the pieces
-- because a stretch of foreign text is one block across item boundaries
-- (step 5.3).
module Convert.Render
    ( OutputFormat (..)
    , renderItems
    ) where

import Convert.Grammar.Sentence (SpellItem)
import Convert.Render.Unicode (renderUnicodeItems)
import Convert.Render.Wylie (renderWylie)
import Data.Text (Text)

-- | The script used for 'renderItems' conversion.
data OutputFormat
    = OutUnicode
    | OutWylie
    deriving (Show, Eq)

-- | Convert every item to an 'OutputFormat' spelling, preserving everything.
-- A run prints from its stacks (REFACTOR-AST §6): the Unicode arm lays the
-- fields of each stack out in the canonical order, the Wylie arm first asks
-- 'notate' how the stacks are written and then only places the signs. Neither
-- arm reads a token's slice, calls the parser or decides a role.
renderItems :: OutputFormat -> [SpellItem] -> Text
renderItems OutUnicode = renderUnicodeItems
renderItems OutWylie = renderWylie
