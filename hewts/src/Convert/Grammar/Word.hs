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
    , vowelSlot
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( Consonant (Ca)
  , Token (..)
  , TokenCanonical (TcConsonant)
  )
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec as MP

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

-- | 'ImplicitVowel' is ours, not the book's: in Wylie the letter @a@ is always
-- written where a vowel would be, and it never reaches the output. The book
-- describes Tibetan spelling, where the @a@ is simply not written and so needs
-- no position of its own. We keep the letter in the word - the input stays
-- covered - and mark it, so the renderer knows to skip it.

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

-- | The vowel slot. In both spellings the vowel itself is the same letter; what
-- differs is that Wylie writes the @a@ out. It is kept in the word - the input
-- stays covered - and marked 'ImplicitVowel', which prints nothing.
vowelSlot :: Spelling -> Parser TibetanWord
vowelSlot Tibetan = mark Vowel GP.pVowel
vowelSlot Wylie = MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel pImplicitA]

-- | The letter @a@ where a vowel would be: written in every Wylie syllable and
-- never printed.
pImplicitA :: Parser Token
pImplicitA = MP.satisfy isImplicitA

isImplicitA :: Token -> Bool
isImplicitA Token{tokenCanonical = TcConsonant Ca} = True
isImplicitA _ = False
