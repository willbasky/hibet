-- | How a parsed syllable is written in Wylie: the notation of REFACTOR-AST
-- §4.1 and §6.1.
--
-- Wylie has no sign for a subjoined letter and none for a stack boundary: a
-- stack is held together by writing its letters solid, and a boundary is held
-- by a dot where a solid writing would read as one stack. Both decisions are
-- about the writing, not about the syllable, so they live here and the
-- renderer only lays the pieces out (§2 П4, П6).
--
-- The letter combinations that may stand solid are the book's (rules 4.8–4.10),
-- read from the grammar's own tables ('rootSubfixRows' of 4.9, the superfix
-- root sets of 4.8 and 'superfixCombos' of 5.1): the same data the parse uses,
-- so the two cannot drift apart. A combination outside them is written with an
-- explicit @+@, which is longer but reads back as the same stack (§6.1).
--
-- The mnemonics are joined with @+@ inside the lookup table only; that key
-- never reaches the output.
module Convert.Wylie.Tables
    ( Join (..)
    , notate
    , solidStacks
    , caretFold
    ) where

import Convert.Grammar.Constraint.Constraint09 (rootSubfixRows)
import Convert.Grammar.Superfix
    ( lSuperfixRoots
    , rSuperfixRoots
    , sSuperfixRoots
    , superfixCombos
    )
import Convert.Grammar.Syllable
    ( Stack (..)
    , Subfixes (..)
    , Syllable (..)
    , VowelSlot (..)
    , finalsTokens
    )
import Convert.Token
    ( Consonant (..)
    , FinalMark (FMCaret)
    , Token
    , TokenCanonical (..)
    , tokenCanonical
    )
import Convert.Tokenizer.Wylie (wylieExpansions, wylieOf)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | How a sign joins the one written before it: solid in the same stack, with
-- an explicit @+@, or after a dot that parts the stacks.
data Join
    = JSolid
    | JPlus
    | JDot
    deriving (Show, Eq)

-- | The Wylie spelling of a syllable, sign by sign, each with the way it joins
-- the sign before it. The first sign of the syllable is always 'JSolid'.
notate :: Syllable -> Seq (Join, Text)
notate syl = Seq.fromList (go Nothing (toList (sylStacks syl)))
    where
        go _ [] = []
        go prev (st : rest) = stackPieces prev st <> go (Just st) rest

-- | The pieces of one stack, with the first piece joined to the stack before
-- it: a dot when writing the two solid would read as one stack.
stackPieces :: Maybe Stack -> Stack -> [(Join, Text)]
stackPieces prev st = case pieces st of
    [] -> []
    (_, first) : rest -> (boundaryJoin prev st, first) : rest

boundaryJoin :: Maybe Stack -> Stack -> Join
boundaryJoin Nothing _ = JSolid
boundaryJoin (Just prev) st
    | not (null (vowelPieces prev)) = JSolid
    | null (letterTexts prev) || null (letterTexts st) = JSolid
    | aChenStart st = JDot
    | solid (last (letterTexts prev) : letterTexts st) = JDot
    | otherwise = JSolid

-- | A stack that opens on an a-chen: written solid after a letter it would be
-- read as part of that stack, so it takes a dot (ཨཨ -> @a.a@, the THL canon).
aChenStart :: Stack -> Bool
aChenStart st = case (stSuperfix st, stRoot st) of
    (Nothing, Just root) -> tokenCanonical root == TcConsonant Ca
    _ -> False

-- | The signs of a stack in the canonical order of its fields: the letters,
-- the caret, the vowel slot, the finals.
pieces :: Stack -> [(Join, Text)]
pieces st = letters <> caret <> vowelPieces st <> finals
    where
        texts = letterTexts st
        letterJoin
            | solid texts = JSolid
            | otherwise = JPlus
        letters = case texts of
            [] -> []
            first : more -> (JSolid, first) : [(letterJoin, txt) | txt <- more]
        caret
            | caretFoldOf st /= Nothing = []
            | Just _ <- stCaret st = [(JSolid, "^")]
            | otherwise = []
        finals = [(JSolid, mnemonic tok) | tok <- finalsTokens (stFinals st)]

-- | The vowel slot: the implicit @a@ is a letter of the writing, the written
-- signs are their mnemonics, and a long letter is one mnemonic for the two
-- signs that spell it (@ཱ@ + @ི@ -> @I@).
vowelPieces :: Stack -> [(Join, Text)]
vowelPieces st = case stVowel st of
    Nothing -> []
    Just VowelImplicit
        | aChenDropped st -> []
        | any (T.isPrefixOf "\\") (letterTexts st) -> []
        -- The letter @a@ is the vowel itself: a subjoined sanskrit a closes
        -- the stack just as the a-chen root does (s+a, not s+aa).
        | mnemonicSpelledA (letterTexts st) -> []
        | otherwise -> [(JSolid, "a")]
    Just (VowelWritten tok rest) -> case map tokenCanonical (tok : rest) of
        [] -> []
        canons -> case lookup canons longForms of
            Just long -> [(JSolid, long)]
            Nothing -> case map mnemonic (tok : rest) of
                [] -> []
                first : more -> (JSolid, first) : [(JPlus, txt) | txt <- more]

-- | The vowel sign runs the tokenizer spells as one mnemonic (@I@ @U@ @E@ @O@
-- @-I@): the same data it reads back with.
longForms :: [([TokenCanonical], Text)]
longForms =
    [ (canons, txt)
    | (txt, canons) <- wylieExpansions
    , case canons of
        [TcVowel _, TcVowel _] -> True
        _ -> False
    ]

-- | Whether the letters already spell the vowel @a@ (the last one is the
-- mnemonic @a@ of an a-chen or of a subjoined sanskrit a).
mnemonicSpelledA :: [Text] -> Bool
mnemonicSpelledA texts = case reverse texts of
    "a" : _ -> True
    _ -> False

-- | The letters of a stack in writing order. An a-chen that only carries a
-- vowel sign is left out (Wylie writes the vowel alone, @ཨི@ -> @i@), and a
-- folded caret replaces the letter it folds.
letterTexts :: Stack -> [Text]
letterTexts st = case caretFoldOf st of
    Just txt -> [txt]
    Nothing ->
        [ mnemonic tok
        | tok <-
            maybe [] pure (stSuperfix st)
                <> maybe [] pure (stRoot st)
                <> subfixTokens (stSubfixes st)
        , not (dropRoot tok)
        ]
    where
        dropRoot tok = aChenDropped st && stRoot st == Just tok

-- | An a-chen that is the whole stack and only carries a vowel sign: the vowel
-- alone restores it (@ཨི@ -> @i@). With a subjoined letter below it the letter
-- must be written, or the stack would read as a different one (@ཨྱོ@ -> @a+yo@).
aChenDropped :: Stack -> Bool
aChenDropped st = case (stSuperfix st, stRoot st, stSubfixes st, stVowel st) of
    (Nothing, Just tok, NoSubfix, Just (VowelWritten _ _)) -> tokenCanonical tok == TcConsonant Ca
    _ -> False

-- | The folded caret: a single letter under a caret whose mnemonic the
-- tokenizer writes as one letter (@ཕ༹@ -> @f@, @བ༹@ -> @v@). The fold replaces
-- the letter and the caret; the implicit @a@ still follows it (@fa@), as the
-- corpora spell ཕ༹.
caretFoldOf :: Stack -> Maybe Text
caretFoldOf st = case (stSuperfix st, stRoot st, stSubfixes st, stCaret st) of
    (Nothing, Just root, NoSubfix, Just caret)
        | tokenCanonical caret == TcFinal FMCaret -> lookup (mnemonic root) caretFold
    _ -> Nothing

-- | The Wylie mnemonic of one sign as the renderer spells it.
mnemonic :: Token -> Text
mnemonic tok = fromMaybe "" (wylieOf (tokenCanonical tok))

subfixTokens :: Subfixes -> [Token]
subfixTokens NoSubfix = []
subfixTokens (OneSubfix a) = [a]
subfixTokens (TwoSubfix a b) = [a, b]
subfixTokens (ThreeSubfix a b c) = [a, b, c]

-- | The caret folds the tokenizer knows: a spelling that stands for a letter
-- and a caret (@f@, @v@).
caretFold :: [(Text, Text)]
caretFold =
    [ (mnemonicOf c, txt)
    | (txt, [TcConsonant c, TcFinal FMCaret]) <- wylieExpansions
    ]

-- | The Wylie letter of a consonant, as the solid-run keys are built.
mnemonicOf :: Consonant -> Text
mnemonicOf c = fromMaybe "" (wylieOf (TcConsonant c))

-- | The letter runs that may stand solid: a root from rule 4.9's rows with one
-- or two subfixes (the grammar's subjoin run allows a second of @y w r@ and
-- never @l@), and a superfix from 4.8 with its root and at most one subfix
-- (5.1). A run outside this set is written with @+@.
solidStacks :: Set Text
solidStacks = Set.fromList (pairs <> triples <> superfixPairs <> superfixTriples)
    where
        pairs = [key [root, sub] | (sub, roots) <- rootSubfixRows, root <- roots]
        triples =
            [ key [root, sub, second]
            | (sub, roots) <- rootSubfixRows
            , root <- roots
            , second <- [Cy, Cw, Cr]
            ]
        superfixPairs = [key [sf, root] | (sf, roots) <- superfixTable, root <- roots]
        superfixTriples =
            [ key [sf, root, sub]
            | (sf, _) <- superfixTable
            , (root, sub) <- superfixCombos sf
            ]
        key :: [Consonant] -> Text
        key = T.intercalate "+" . map mnemonicOf
        superfixTable =
            [ (Cr, rSuperfixRoots)
            , (Cl, lSuperfixRoots)
            , (Cs, sSuperfixRoots)
            ]

solid :: [Text] -> Bool
solid texts = Set.member (T.intercalate "+" texts) solidStacks
