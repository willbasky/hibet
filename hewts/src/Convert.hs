module Convert
    ( splitSentences
    , syllables
    , SpellItem (..)
    , pSentence
    ) where

import Convert.Grammar (parseEither)
import Convert.Sentence (SpellItem (..), pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer (tokenizeUnicode)
import Data.Text (Text)
import qualified Data.Text as T

-- | Split Tibetan text into a structured token stream: every token is
-- preserved, syllables are recognized via the 37 grammar structures,
-- digits become 'Number', punctuation and whitespace become 'Punct',
-- and anything unrecognized becomes 'Other'.
splitSentences :: Text -> Either Text [SpellItem]
splitSentences = parseEither pSentence . tokenizeUnicode

-- | Extract only the recognized syllables from Tibetan text, each as its
-- raw spelling (e.g. @མཆོག་དེ ' ->
-- @["མཆོག","དེ"]@).
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure [T.concat (map tokenRaw ts) | Syllable ts <- items]