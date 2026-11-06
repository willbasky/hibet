-- | The Unicode arm of printing: a run is the fields of its stacks in the
-- canonical order (REFACTOR-AST §6), and everything outside a run is spelled
-- per sign. Nothing here reads the slice a sign was written as, except the
-- §11.2 echo of a mark the grammar could not place.
module Convert.Render.Unicode
    ( renderUnicodeItems
    , renderSyllable
    , isOrphanStack
    , isUnicodeStack
    ) where

import Convert.Grammar.Sentence (SpellItem (..))
import Convert.Grammar.Syllable
    ( Stack (..)
    , Subfixes (..)
    , Syllable (..)
    , VowelSlot (..)
    , finalsTokens
    , stackTokens
    )
import Convert.Render.Token (spellToken)
import Convert.Token
    ( Token
    , TokenCanonical (..)
    , TokenSource (..)
    , toSubjoined
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Token.Unicode (unicodeOf)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert every item to its Unicode spelling, preserving everything.
renderUnicodeItems :: [SpellItem] -> Text
renderUnicodeItems = T.concat . map renderItem
    where
        renderItem (SyllableItem s) = renderSyllable s
        renderItem (InvalidSyllableItem ts _) =
            -- A run the grammar could not place. Its letters are still
            -- converted one by one: a lone sign vowel is a letter like any
            -- other (ཱ -> A), and nothing the grammar did not recognize may
            -- hide, join or skip a letter. A final mark in such a run is the
            -- exception: no letter claimed it, so there is nothing to convert
            -- it *to* and it stands as it was written (Mi -> M, not ཾ) - the
            -- same thing the reference does with a character that is neither a
            -- letter to it nor special.
            rawTokens ts
        renderItem (Number ts) = T.concat (map (spellToken unicodeOf) ts)
        renderItem (Punct ts) = T.concat (map (spellToken unicodeOf) ts)
        renderItem (Other ts) = T.concat (map (spellToken unicodeOf) ts)

        rawTokens = T.concat . map echo

        -- A mark of a run the grammar could not place echoes the character it
        -- was written as: the reference leaves a final with no letter to close
        -- as it stands (Mi -> M, not ཾ), and a sign may have no cross-script
        -- name at all (REFACTOR-AST §9, шаг 3.3: the one exception to П3).
        echo tok
            | isFinalMark tok = tokenRaw tok
            | otherwise = spellToken unicodeOf tok

        isFinalMark tok = case tokenCanonical tok of
            TcFinal _ -> True
            _ -> False

-- | The Unicode spelling of a syllable: the fields of its stacks in the
-- canonical order (REFACTOR-AST §6). Nothing here reads the slice a token was
-- written as.
renderSyllable :: Syllable -> Text
renderSyllable = T.concat . map stackText . toList . sylStacks

-- | One stack: superfix, root, subfixes, caret, vowel, finals. The root
-- prints below the line under a superfix when the stack reaches a vowel
-- (rka -> རྐ, rk -> ར + ཀ), and always when the token is a subjoined glyph
-- already.
stackText :: Stack -> Text
stackText st =
    aChen
        <> maybe "" glyph (stSuperfix st)
        <> maybe "" rootGlyph (stRoot st)
        <> T.concat (map subGlyph (subfixList (stSubfixes st)))
        <> maybe "" glyph (stCaret st)
        <> vowelText (stVowel st)
        <> T.concat (map glyph (finalsTokens (stFinals st)))
    where
        -- A stack opened by a vowel has no letter to hang the sign on: Wylie
        -- writes the sign alone (@E@), the glyphs need the a-chen (ཨི). Only a
        -- Wylie-sourced stack gets the letter: a glyph run that splits a second
        -- vowel off its root also leaves a rootless stack, and writing ཨ there
        -- would turn ⊙ཱེ into ⊙ཱ⊙ེ on every pass. The stack's shape cannot
        -- tell the two apart, so this is the one place the renderer reads the
        -- token's source (REFACTOR-AST §9, шаг 3.3: exception in §11).
        aChen
            | isOrphanStack st = case stVowel st of
                Just VowelImplicit -> "ཨ"
                Just (VowelWritten tok _) | tokenSource tok == TsWylie -> "ཨ"
                _ -> ""
            | otherwise = ""
        -- A stack that reaches no vowel is no superfix stack: the base prints
        -- as the full letter the writer wrote (རཏ is ར + ཏ, རྟ is written
        -- @rta@). The grammar never builds the vowel-less shape (step A of R1).
        rootGlyph tok = case tokenCanonical tok of
            TcConsonant _ | stSuperfix st /= Nothing -> subGlyph tok
            _ -> glyph tok

subGlyph :: Token -> Text
subGlyph tok = case tokenCanonical tok of
    -- A Wylie letter writes its subjoined form; a glyph sign already is one.
    TcConsonant c -> fromMaybe (glyph tok) (unicodeOf . TcSubConsonant =<< toSubjoined c)
    _ -> glyph tok

vowelText :: Maybe VowelSlot -> Text
vowelText Nothing = ""
vowelText (Just VowelImplicit) = ""
vowelText (Just (VowelWritten tok rest)) = glyph tok <> T.concat (map glyph rest)

glyph :: Token -> Text
glyph tok = fromMaybe "" (unicodeOf (tokenCanonical tok))

isOrphanStack :: Stack -> Bool
isOrphanStack st = stRoot st == Nothing && stSuperfix st == Nothing

-- | Whether every sign of the stack was read from glyphs. A stack opened by
-- a Wylie sign has the same headless shape (@i@, @E@, @oM@) but is no
-- escape case: only a glyph run the grammar left without a letter is
-- escaped (REFACTOR-AST §6.2).
isUnicodeStack :: Stack -> Bool
isUnicodeStack = all ((== TsUnicode) . tokenSource) . toList . stackTokens

subfixList :: Subfixes -> [Token]
subfixList NoSubfix = []
subfixList (OneSubfix a) = [a]
subfixList (TwoSubfix a b) = [a, b]
subfixList (ThreeSubfix a b c) = [a, b, c]
