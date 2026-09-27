-- | A parsed Tibetan word: the tokens of one syllable, each marked with the
-- place it occupies in the word.
--
-- The vocabulary is the book's own, from "Research on Tibetan Spelling Formal
-- Language and Automata with Application" (Nyima Tashi, Science Press + Springer,
-- 2019), section 4.1, Definitions 4.1-4.8. Our 37 structures come from the same
-- book (its "Tibetan spelling grammar 4.1-4.20", section 4.2), so the marks use the
-- terms the rules themselves use.
--
-- The mark lives in the list rather than in 'Token': a letter is superfix or
-- subfix because of where it stands, not because of what it is - the same @r@ is a
-- root in one word and a subfix in another. Leaving 'Token' alone also leaves its
-- invariants alone (input coverage, 'TokenIssue').
module Convert.Grammar.Word
    ( Position (..)
    , TibetanWord
    , mark
    , subfixMarks
    , caretMark
    ) where

import Convert.Grammar.Parser (Parser)
import Convert.Token (Token)
import Data.Maybe (maybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | The place a character occupies in a Tibetan word.
--
-- 'Final' is ours rather than the book's: the book files final marks with the
-- other signs (Def 4.9) instead of giving them a place of their own, but we
-- need to tell them apart - two identical finals in a row are an error, and the
-- order of assembly depends on it.
data Position
    = Root
    | Prefix
    | Superfix
    | Subfix
    | Suffix
    | Postfix
    | Vowel
    | Final
    | ImplicitVowel
    deriving (Show, Eq, Ord)

-- | A Tibetan word (Def 4.10) as a flat list of marked tokens.
--
-- Flat on purpose. A nested "stack" type would have to know the book's eleven
-- word shapes, and the reference accepts shapes the book does not list
-- (@g.yag@ gives གཡག, three roots in a row), so a closed type would need an
-- escape hatch anyway - and then the shape rules would live in the types as
-- well as in the tables.
--
-- A 'Seq' rather than a list because the word is built by appending pieces
-- (a prefix, a root, a subfix, a vowel) and read from both ends by the legality
-- rules of wave 3.4. Be aware that this buys clarity more than speed: a word
-- holds one to six letters, and at that size 'Seq' and a list append in the
-- same handful of steps. The real cost in this layer is elsewhere - see
-- 'Convert.Sentence.pStructure', which probes all 37 structures in lookahead,
-- so every syllable is parsed 37 times over.
type TibetanWord = Seq (Position, Token)

-- | Give a parser a position, so it yields a one-element word. Marking at the
-- point of binding keeps the token parsers themselves unchanged: they parse
-- letters, they know nothing about words.
mark :: Position -> Parser a -> Parser (Seq (Position, a))
mark position parser = do
    value <- parser
    pure (Seq.singleton (position, value))

-- | The subfix letters of one stack, marked in order.
subfixMarks :: [Token] -> TibetanWord
subfixMarks = Seq.fromList . map (Subfix,)

-- | The one caret that survives a subjoining scan, marked 'Final', or nothing
-- when the scan kept no caret.
caretMark :: Maybe Token -> TibetanWord
caretMark = maybe mempty (Seq.singleton . (Final,))
