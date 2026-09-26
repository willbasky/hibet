module Convert
    ( splitSentences
    , splitSentencesWith
    , syllables
    , renderItems
    , OutputFormat (..)
    , SpellItem (..)
    , pSentence
    ) where

import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Parser (parseEither)
import Convert.Sentence (SpellItem (..), pSentence)
import Convert.Token
    ( TokenCanonical (..)
    , TokenSource (..)
    , UnknownMark (..)
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie (wylieOf)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Split Tibetan text into a structured token stream: every token is
-- preserved, syllables are recognized via the 37 grammar structures,
-- digits become 'Number', punctuation and whitespace become 'Punct',
-- and anything unrecognized becomes 'Other'.
splitSentences :: Text -> Either Text [SpellItem]
splitSentences input =
    case splitSentencesWith input of
        Left err -> Left err
        Right (items, _) -> Right items

-- | Like 'splitSentences', but also hands back what the tokenizer noticed
-- while reading the text.
splitSentencesWith :: Text -> Either Text ([SpellItem], Diagnostics)
splitSentencesWith input = do
    let (tokens, diagnostics) = tokenizeUnicode input
    items <- parseEither pSentence tokens
    pure (items, diagnostics)

-- | Extract only the recognized syllables from Tibetan text, each as its
-- raw spelling (e.g. @མཆོག་དེ ' ->
-- @["མཆོག","དེ"]@).
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure [T.concat (toList (fmap (tokenRaw . snd) ts)) | Syllable ts <- items]

-- | The script used for 'renderItems' conversion.
data OutputFormat
    = OutUnicode
    | OutWylie
    deriving (Show, Eq)

-- | Convert every token to an 'OutputFormat' spelling, preserving
-- everything: same-script tokens keep their exact raw spelling (including
-- aliases), cross-script tokens become canonical representatives, and
-- tokens without a cross-script spelling fall back to their raw spelling.
renderItems :: OutputFormat -> [SpellItem] -> Text
renderItems fmt = T.concat . map renderItem
    where
        renderItem (Syllable ts) = T.concat (toList (fmap (renderToken fmt . snd) ts))
        renderItem (Number ts) = T.concat (map (renderToken fmt) ts)
        renderItem (Punct ts) = T.concat (map (renderToken fmt) ts)
        renderItem (Other ts) = T.concat (map (renderToken fmt) ts)

        renderToken fmt tok
            | tokenSource tok == sourceOf fmt = tokenRaw tok
            -- a token without a cross-script spelling falls back to the text it
            -- stands for: the content of a bracket block, the character of an
            -- escape, and otherwise the raw slice
            | otherwise = fromMaybe (unknownText tok) (scriptOf fmt (tokenCanonical tok))

        unknownText tok =
            case tokenCanonical tok of
                TcUnknown (UnknownMark text) -> text
                _ -> tokenRaw tok

        sourceOf OutUnicode = TsUnicode
        sourceOf OutWylie = TsWylie

        scriptOf OutUnicode = unicodeOf
        scriptOf OutWylie = wylieOf
