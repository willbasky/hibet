module Convert
    ( splitSentences
    , splitSentencesWith
    , syllables
    , renderItems
    , legality
    , documentDiagnostics
    , OutputFormat (..)
    , SpellItem (..)
    , pSentence
    ) where

import Convert.Diagnostic (Diagnostics, addDiagnostic, noTibetanCharacters)
import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Syllable
    ( Stack (..)
    , Subfixes (..)
    , VowelSlot (..)
    , finalsTokens
    , stackTokens
    )
import Convert.Sentence (SpellItem (..), Syllable (..), pSentence)
import Convert.Token
    ( FinalMark (FMCaret)
    , SpaceMark (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , UnknownMark (..)
    , isToplessMark
    , mkSpan
    , toSubjoined
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie (wylieOf)
import Convert.Wylie.Tables (Join (..), notate)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

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
-- while reading the text and what the grammar's constraint windows found for
-- each syllable run.
splitSentencesWith :: Text -> Either Text ([SpellItem], Diagnostics)
splitSentencesWith input = do
    let (tokens, diagnostics) = tokenizeUnicode input
    items <- parseEither pSentence tokens
    pure (items, diagnostics <> legality items)

-- | The spelling warnings for a whole parsed sentence: the 'sylDiags' of
-- every syllable run, in source order.
legality :: [SpellItem] -> Diagnostics
legality = foldMap itemDiags
    where
        itemDiags (SyllableItem s) = sylDiags s
        itemDiags (InvalidSyllableItem _ d) = d
        itemDiags _ = mempty

-- | The document-level warning: the whole input holds no single element the
-- converter reads as Tibetan. A syllable (sound or broken), a number and a
-- sign all count; a bracketed block of foreign text and a stray letter or
-- special mark do not, so a document of nothing but those reads as empty and
-- is blamed whole. The span is the whole input and no word is quoted - the
-- warning is about the document, not about any run in it.
--
-- This is the W->U arm of the reference, whose @units == 0@ control counts a
-- letter, a number or a sign of punctuation; the W->U assembly calls it (see
-- @Test.Parity.convertW2U@). The predicate is about positions the tokenizers
-- already set, not a second walk over the tokens.
documentDiagnostics :: Text -> [SpellItem] -> Diagnostics
documentDiagnostics input items
    | hasTibetanElement items = mempty
    | otherwise =
        addDiagnostic
            (noTibetanCharacters (mkSpan 0 (fromIntegral (T.length input))))
            mempty

-- | Whether the parsed sentence holds at least one element the converter reads
-- as Tibetan: a syllable (sound or broken), a number, or any token that took a
-- canonical payload (a sign, a punctuation mark, a space, a symbol). A token
-- that stayed 'TcUnknown' - an ASCII letter, an unmatched special mark, the
-- content of a bracket block - is no such element.
hasTibetanElement :: [SpellItem] -> Bool
hasTibetanElement = any itemHoldsElement
    where
        itemHoldsElement (SyllableItem _) = True
        itemHoldsElement (InvalidSyllableItem _ _) = True
        itemHoldsElement (Number _) = True
        itemHoldsElement (Punct ts) = any isElement ts
        itemHoldsElement (Other ts) = any isElement ts
        isElement t = case tokenCanonical t of
            TcUnknown _ -> False
            _ -> True

-- | Extract only the recognized syllables from Tibetan text, each in the
-- canonical spelling (REFACTOR-AST Q8: the same fields, in the same order, as
-- 'renderItems' 'OutUnicode' prints - so @syllables "བཾྲ"@ is @"བྲཾ"@).
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure [renderSyllable s | SyllableItem s <- items]

-- | The script used for 'renderItems' conversion.
data OutputFormat
    = OutUnicode
    | OutWylie
    deriving (Show, Eq)

-- | One piece of the Wylie output, by what the reference's escaping walker
-- does with the character behind it: a spelling of its own ('Plain'), an
-- ASCII space (it spells @_@ on its own and passes through inside a block),
-- a character of foreign text ('Foreign', the block's own), or a \\uXXXX
-- escape of a character Wylie does not name ('Escaped').
data Piece = Plain Text | Space Text | Foreign Text | Escaped Text

-- | The reference's foreign-text blocks (@toWylie@ in escaping mode): a
-- maximal stretch of foreign characters prints between brackets, the brackets
-- inside it escaped (@\[with embedded\[\]\]@). An ASCII space joins such a
-- block only where the block already holds foreign text - that is what keeps
-- the leading spaces of "  2tix!" inside one block - and prints as @_@ where
-- nothing foreign follows it. An escape joins a block foreign text has
-- already opened, and prints outside every block that has not opened yet
-- (@x༉@ -> @[x༉]@, @༉x@ -> @༉[x]@).
renderForeign :: [Piece] -> Text
renderForeign [] = ""
renderForeign (p : rest)
    -- every branch must consume at least this head, or the walk repeats it
    | not (bracketable p) = openText p <> renderForeign rest
    | any isForeign block =
        -- an escape before the block's first foreign character stands where
        -- the reference writes it: outside
        let (outside, inner) = span isEscaped block
         in T.concat (map openText outside) <> openBlock inner <> renderForeign afterBlock
    | otherwise = T.concat (map openText block) <> renderForeign afterBlock
    where
        (more, afterBlock) = span bracketable rest
        block = p : more
        bracketable (Plain _) = False
        bracketable _ = True
        isForeign (Foreign _) = True
        isForeign _ = False
        isEscaped (Escaped _) = True
        isEscaped _ = False
        -- inside a block every character prints as itself
        blockText (Plain t) = t
        blockText (Space t) = t
        blockText (Foreign t) = t
        blockText (Escaped t) = t
        openBlock pieces = "[" <> escapeBrackets (T.concat (map blockText pieces)) <> "]"
        escapeBrackets = T.replace "]" "\\]" . T.replace "[" "\\["
        -- outside a block an ASCII space is the reference's own spelling of it
        openText (Space _) = "_"
        openText piece = blockText piece

-- | The reference's escape of a character Wylie does not name: @\u@ and four
-- lowercase hex digits (the reference's @formatHex@).
tibetanEscape :: Char -> Text
tibetanEscape c = "\\u" <> T.pack (replicate (4 - length digits) '0' <> digits)
    where
        digits = showHex (ord c) ""

-- | Convert every item to an 'OutputFormat' spelling, preserving everything.
-- A run prints from its stacks (REFACTOR-AST §6): the Unicode arm lays the
-- fields of each stack out in the canonical order, the Wylie arm first asks
-- 'notate' how the stacks are written and then only places the signs. Neither
-- arm reads a token's slice, calls the parser or decides a role.
--
-- The two arms walk the items differently. The Unicode arm prints one item at
-- a time. The Wylie arm walks the items the way the reference walks the
-- characters of its input, because a stretch of foreign text is one block
-- across item boundaries: a run, a number and a sign of punctuation spell in
-- Wylie, and every other character joins the bracket block the reference
-- writes foreign text in (step 5.3).
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
        rootGlyph tok = case tokenCanonical tok of
            TcConsonant _ | stSuperfix st /= Nothing, stVowel st /= Nothing -> subGlyph tok
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

renderItems :: OutputFormat -> [SpellItem] -> Text
renderItems fmt items = case fmt of
    OutUnicode -> T.concat (map renderItem items)
    OutWylie -> renderForeign (concatMap wyliePieces items)
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
            rawTokens OutUnicode ts
        renderItem (Number ts) = T.concat (map (renderToken OutUnicode) ts)
        renderItem (Punct ts) = T.concat (map (renderToken OutUnicode) ts)
        renderItem (Other ts) = T.concat (map (renderToken OutUnicode) ts)

        -- The Wylie pieces of one item: a run spells as a whole through the
        -- notation, a number and a sign spell per token - and a token no table
        -- reads as Tibetan is a character of foreign text.
        --
        -- A mark at the head of a run with no letter to attach to never
        -- reaches the notation: the reference escapes such a character
        -- (@needs a top symbol@), so an orphan prefix becomes escapes and the
        -- rest of the run spells as before (wave 5.4).
        --
        -- shortcut: an ASCII space the run's own boundary closes with is a
        -- 'Punct' item of its own now, so the reference shape of a space
        -- between a run and foreign text (@bla[ x]@) is not produced; the
        -- Wylie arm spells that space @_@ (@bla_[x]@). Split the space off
        -- the boundary when a corpus needs the other shape.
        wyliePieces :: SpellItem -> [Piece]
        wyliePieces (SyllableItem s) = case toList (sylStacks s) of
            (st : rest)
                | isOrphanStack st
                , isUnicodeStack st ->
                    orphanEscapes (toList (stackTokens st))
                        <> [runPiece (notateText (s{sylStacks = foldMap pure rest}))]
            _ -> [runPiece (notateText s)]
        wyliePieces (InvalidSyllableItem ts _) =
            let (orphans, rest) = span isToplessMark ts
             in orphanEscapes orphans <> [runPiece (rawTokens OutWylie rest)]
        wyliePieces (Number ts) = map wyliePiece ts
        wyliePieces (Punct ts) = map wyliePiece ts
        wyliePieces (Other ts) = map wyliePiece ts

        -- A run whose whole spelling is an escape stands for the characters
        -- one by one, so it joins a block like any other escape (x + 0F6B ->
        -- [xཫ]) instead of closing it.
        runPiece txt
            | T.isPrefixOf "\\" txt = Escaped txt
            | otherwise = Plain txt

        -- One escape per source character of an orphan prefix. The five
        -- forms the reference rewrites into components before reading
        -- anything (jsewts.js:1784-1788) escape component by component, so
        -- they read back into the same glyph; every other orphan escapes as
        -- the character it was written - a continuation of the group carries
        -- no character of its own.
        orphanEscapes :: [Token] -> [Piece]
        orphanEscapes = go False
            where
                go _ [] = []
                go rewritten (tok : rest)
                    | T.null (tokenRaw tok) =
                        component rewritten tok <> go rewritten rest
                    | otherwise =
                        let c = T.head (tokenRaw tok)
                         in if c `elem` rewrittenComponents
                                then component True tok <> go True rest
                                else [Escaped (tibetanEscape c)] <> go False rest

                component True tok = maybe [] escapeUnit (unicodeOf (tokenCanonical tok))
                component False _ = []

                escapeUnit txt = case T.uncons txt of
                    Just (c, _) -> [Escaped (tibetanEscape c)]
                    Nothing -> []

        -- The deprecated forms jsewts rewrites into components before its
        -- walk: an orphan of one of them escapes as its components. The two
        -- long vowels (0F73, 0F75) it leaves whole and they escape as
        -- written.
        rewrittenComponents :: String
        rewrittenComponents = "\x0f76\x0f77\x0f78\x0f79\x0f81"

        wyliePiece tok
            | isAsciiSpace tok = Space (tokenRaw tok)
            -- a spelling that is already an escape stands for the whole
            -- character, so it is one: it joins a block foreign text opened
            -- and opens no block of its own
            | isEscapeSpelling tok = Escaped (renderToken OutWylie tok)
            -- a Tibetan character no table names: the reference escapes it
            -- instead of opening a bracket block with it (wave 5.4)
            | isUnnameableGlyph tok = Escaped (escapeGlyphText (unknownText tok))
            | isForeignChar tok = Foreign (renderToken OutWylie tok)
            | otherwise = Plain (renderToken OutWylie tok)

        escapeGlyphText txt = case T.uncons txt of
            Just (c, _) -> tibetanEscape c
            Nothing -> ""

        isAsciiSpace tok = tokenCanonical tok == TcSpace SMSpace

        -- Foreign text is a character no table reads as Tibetan. A line break
        -- is not one: the reference ends a block at it and prints it as
        -- written.
        isForeignChar tok = case tokenCanonical tok of
            TcUnknown (UnknownMark text) -> not (T.isInfixOf "\n" text) && not (T.isInfixOf "\r" text)
            _ -> False

        -- A Tibetan character no table names (༉, ࿂): the reference escapes
        -- it in the Tibetan plane and only the rest of the unknown text is
        -- foreign.
        isUnnameableGlyph tok = case tokenCanonical tok of
            TcUnknown (UnknownMark text) -> isTibetanGlyph text
            _ -> False

        isTibetanGlyph text = case T.uncons text of
            Just (c, _) -> c >= '\x0f00' && c <= '\x0fff'
            Nothing -> False

        -- Whether a token's Wylie spelling is really an escape (@\uXXXX@).
        -- EWTS has no mnemonic for some characters (the Balti ཫ ཬ, the
        -- half-numbers, the signs), so they spell as an escape that already
        -- stands for the whole character.
        isEscapeSpelling tok = T.isPrefixOf "\\" (renderToken OutWylie tok)

        rawTokens ofmt = T.concat . map (echo ofmt)

        -- A mark of a run the grammar could not place echoes the character it
        -- was written as: the reference leaves a final with no letter to close
        -- as it stands (Mi -> M, not ཾ), and a sign may have no cross-script
        -- name at all (REFACTOR-AST §9, шаг 3.3: the one exception to П3).
        echo ofmt tok
            | isFinalMark tok = tokenRaw tok
            | otherwise = renderToken ofmt tok

        isFinalMark tok = case tokenCanonical tok of
            TcFinal _ -> True
            _ -> False

        notateText :: Syllable -> Text
        notateText = T.concat . map piece . toList . notate
            where
                piece (join, txt) = joinText join <> txt
                joinText JSolid = ""
                joinText JPlus = "+"
                joinText JDot = "."

        renderToken ofmt tok =
            fromMaybe (unknownText tok) (scriptOf ofmt (tokenCanonical tok))

        unknownText tok =
            case tokenCanonical tok of
                TcUnknown (UnknownMark text) -> text
                _ -> tokenRaw tok

        sourceOf OutUnicode = TsUnicode
        sourceOf OutWylie = TsWylie

        scriptOf OutUnicode = unicodeOf
        scriptOf OutWylie = wylieOfToken

        -- The Wylie mnemonic of a token; only tokens outside a stack reach
        -- this (numbers, punctuation, foreign text), so the notation never
        -- goes through here.
        wylieOfToken = wylieOf
