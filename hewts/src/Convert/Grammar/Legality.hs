-- | Spelling legality: whether the letters of a word may stand where they
-- stand. The tokenizers decode the input and the grammar (wave 3) marks each
-- token with the place it occupies, but neither checks that the arrangement is
-- legal - ཀྒ and གྷ both come out marked fine. This layer adds the reference's
-- warnings for each word: subjoined letters the root cannot take, prefixes that
-- cannot lead the letters that follow, two bindu in one stack, a suffix that
-- may not close the word, and the "syllable should probably be" hints.
--
-- The unit of analysis is the *word*: every token between whitespace or
-- sentence punctuation, in source order. A word that fell apart under the
-- grammar still reaches the checker as one piece, because the reference blames
-- the whole word (the "Invalid prefix consonant" for @tgra@ names the whole
-- run, not the piece we parsed). A stack dot, a caret or a plus stays inside
-- its word, so the checker sees the whole run even though the grammar eats
-- those separators without marking them.
--
-- The checker is a pure function of the token list and the parsed sentence: it
-- reads the marks the grammar left and decides, it never pushes into the
-- middle of a parse. Wave 3.4.1 ships the machinery with no rules (every word
-- passes); the rules arrive one wave sub-step at a time into 'checkWord'.
module Convert.Grammar.Legality
    ( CheckedToken
    , CheckedWord
    , checkedStream
    , splitWords
    , checkWord
    , legality
    ) where

import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Word (Position)
import Convert.Sentence (SpellItem (..))
import Convert.Token
    ( Token
    , TokenCanonical (..)
    , tokenCanonical
    )
import Data.Foldable (toList)
import qualified Data.Map.Strict as M

-- | A token together with the position the grammar gave it, or 'Nothing' when
-- the grammar claimed nothing for it (an unparsed fragment, punctuation, a
-- space, or a stack break it consumed without marking). The mark lives in the
-- pair rather than the 'Token', mirroring 'Convert.Grammar.Word'.
type CheckedToken = (Maybe Position, Token)

-- | One word: the checked tokens between whitespace or sentence punctuation,
-- in source order.
type CheckedWord = [CheckedToken]

-- | Flatten the tokens of a parsed sentence into one checked stream: every
-- token of the input with the position the grammar gave it, or 'Nothing' when
-- the grammar claimed nothing. Aligning marks against the *whole* token list
-- rather than the parsed items is what keeps the unmarked stack breaks (the
-- @.@ of @g.yag@, the @+@ of @sat+t+wa@) inside their words.
checkedStream :: [Token] -> [SpellItem] -> [CheckedToken]
checkedStream tokens items = map mark tokens
    where
        mark token = (M.lookup token marks, token)
        marks =
            M.fromList
                [(token, position) | Syllable word <- items, (position, token) <- toList word]

-- | Cut a checked stream into words at whitespace and sentence punctuation
-- tokens (the word boundary of the reference and of wave 3.4's decision).
-- Everything else - stack-break dots, carets, plus signs - stays inside the
-- word, so the checker never splits a word it is meant to blame as a whole.
splitWords :: [CheckedToken] -> [CheckedWord]
splitWords = filter (not . null) . go
    where
        go [] = []
        go stream =
            let (word, rest) = break isBoundary stream
             in word : go (dropWhile isBoundary rest)
        isBoundary (_, token) = case tokenCanonical token of
            TcSpace _ -> True
            TcPunctuation _ -> True
            _ -> False

-- | The reference's warnings for one word. All of 3.4's rules land here, one
-- sub-step at a time; 3.4.1 ships the empty checker so the channel itself is
-- proven before any behavior changes.
checkWord :: CheckedWord -> Diagnostics
checkWord _ = mempty

-- | The spelling diagnostics for a whole parsed sentence.
legality :: [Token] -> [SpellItem] -> Diagnostics
legality tokens = mconcat . map checkWord . splitWords . checkedStream tokens
