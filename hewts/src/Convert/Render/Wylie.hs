-- | The Wylie arm of printing: a run is written through the notation, a
-- number and a sign spell per token, and everything no table reads as
-- Tibetan becomes a block of foreign text. This is the walker of the
-- reference's escaping mode (steps 5.3–5.4): the output is assembled from
-- pieces, not from a stack of characters.
module Convert.Render.Wylie (renderWylie) where

import Convert.Grammar.Sentence (SpellItem (..))
import Convert.Grammar.Syllable (Syllable (..), stackTokens)
import Convert.Render.Token (spellToken, unknownText)
import Convert.Render.Unicode (isOrphanStack, isUnicodeStack)
import Convert.Render.Wylie.Notation (Join (..), notate)
import Convert.Token
    ( SpaceMark (..)
    , Token
    , TokenCanonical (..)
    , UnknownMark (..)
    , isToplessMark
    , tokenCanonical
    , tokenRaw
    )
import Convert.Token.Unicode (unicodeOf)
import Convert.Token.Wylie (wylieOf)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

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

-- | Convert every item to its Wylie spelling, preserving everything. A run
-- spells as a whole through 'notate'; a number and a sign spell per token;
-- and a token no table reads as Tibetan is a character of foreign text.
renderWylie :: [SpellItem] -> Text
renderWylie = renderForeign . concatMap wyliePieces

-- | The Wylie pieces of one item.
--
-- A mark at the head of a run with no letter to attach to never reaches the
-- notation: the reference escapes such a character (@needs a top symbol@), so
-- an orphan prefix becomes escapes and the rest of the run spells as before
-- (wave 5.4).
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
     in orphanEscapes orphans <> [runPiece (rawTokens rest)]
wyliePieces (Number ts) = map wyliePiece ts
wyliePieces (Punct ts) = map wyliePiece ts
wyliePieces (Other ts) = map wyliePiece ts

-- | A run whose whole spelling is an escape stands for the characters one by
-- one, so it joins a block like any other escape (x + 0F6B -> [xཫ]) instead
-- of closing it.
runPiece :: Text -> Piece
runPiece txt
    | T.isPrefixOf "\\" txt = Escaped txt
    | otherwise = Plain txt

-- | One escape per source character of an orphan prefix. The five forms the
-- reference rewrites into components before reading anything
-- (jsewts.js:1784-1788) escape component by component, so they read back into
-- the same glyph; every other orphan escapes as the character it was written
-- - a continuation of the group carries no character of its own.
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

wyliePiece :: Token -> Piece
wyliePiece tok
    | isAsciiSpace tok = Space (tokenRaw tok)
    -- a spelling that is already an escape stands for the whole character,
    -- so it is one: it joins a block foreign text opened and opens no block
    -- of its own
    | isEscapeSpelling tok = Escaped (spellToken wylieOf tok)
    -- a Tibetan character no table names: the reference escapes it instead
    -- of opening a bracket block with it (wave 5.4)
    | isUnnameableGlyph tok = Escaped (escapeGlyphText (unknownText tok))
    | isForeignChar tok = Foreign (spellToken wylieOf tok)
    | otherwise = Plain (spellToken wylieOf tok)
    where
        escapeGlyphText txt = case T.uncons txt of
            Just (c, _) -> tibetanEscape c
            Nothing -> ""

        isAsciiSpace t = tokenCanonical t == TcSpace SMSpace

        -- Foreign text is a character no table reads as Tibetan. A line break
        -- is not one: the reference ends a block at it and prints it as
        -- written.
        isForeignChar t = case tokenCanonical t of
            TcUnknown (UnknownMark text) -> not (T.isInfixOf "\n" text) && not (T.isInfixOf "\r" text)
            _ -> False

        -- A Tibetan character no table names (༉, ࿂): the reference escapes
        -- it in the Tibetan plane and only the rest of the unknown text is
        -- foreign.
        isUnnameableGlyph t = case tokenCanonical t of
            TcUnknown (UnknownMark text) -> isTibetanGlyph text
            _ -> False

        isTibetanGlyph text = case T.uncons text of
            Just (c, _) -> c >= '\x0f00' && c <= '\x0fff'
            Nothing -> False

        -- Whether a token's Wylie spelling is really an escape (@\uXXXX@).
        -- EWTS has no mnemonic for some characters (the Balti ཫ ཬ, the
        -- half-numbers, the signs), so they spell as an escape that already
        -- stands for the whole character.
        isEscapeSpelling t = T.isPrefixOf "\\" (spellToken wylieOf t)

-- | A mark of a run the grammar could not place echoes the character it was
-- written as: the reference leaves a final with no letter to close as it
-- stands (Mi -> M, not ཾ), and a sign may have no cross-script name at all
-- (REFACTOR-AST §9, шаг 3.3: the one exception to П3).
rawTokens :: [Token] -> Text
rawTokens = T.concat . map echo
    where
        echo tok
            | isFinalMark tok = tokenRaw tok
            | otherwise = spellToken wylieOf tok

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
