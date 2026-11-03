-- | How one sign outside a stack is spelled: by the table of its writing, or
-- by the slice it was written as when no table names it. Both output arms use
-- it, so an unknown sign echoes the same way in either script.
module Convert.Render.Token
    ( spellToken
    , unknownText
    ) where

import Convert.Token
    ( Token
    , TokenCanonical (..)
    , UnknownMark (..)
    , tokenCanonical
    , tokenRaw
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)

spellToken :: (TokenCanonical -> Maybe Text) -> Token -> Text
spellToken spell tok = fromMaybe (unknownText tok) (spell (tokenCanonical tok))

unknownText :: Token -> Text
unknownText tok =
    case tokenCanonical tok of
        TcUnknown (UnknownMark text) -> text
        _ -> tokenRaw tok
