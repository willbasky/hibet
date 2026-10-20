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
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Grammar.Syllable (Position (..))
import Convert.Sentence (SpellItem (..), Syllable (..), pSentence)
import Convert.Token
    ( ConSpec (..)
    , Consonant (..)
    , FinalMark (FMCaret)
    , SpaceMark (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , UnknownMark (..)
    , Vowel (VA, V_i, Vi, Vu)
    , isToplessMark
    , mkSpan
    , subjoinOf
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie (wylieOf)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

-- | The stacked consonant combinations the reference Wylie spelling writes
-- plainly (the jsewts @m_tib_stacks@ table): a stack whose letters joined by
-- @'+'@ name one of them is written plainly, and every other stack is
-- written with an explicit @'+'@ before each letter after the first
-- (རྒྱ -> "rgya", དྷ -> "d+ha"). The same table tells when a prefix must be
-- separated from the stack after it by a dot: only when the two could read
-- as one regular stack (གཡུལ -> "g.yul", བརྙ -> "brnya" keeps no dot -
-- b + r + ny is no regular stack).
regularStacks :: Set Text
regularStacks =
    Set.fromList
        [ "b+l"
        , "b+r"
        , "b+y"
        , "c+w"
        , "d+r"
        , "d+r+w"
        , "d+w"
        , "dz+r"
        , "g+l"
        , "g+r"
        , "g+r+w"
        , "g+w"
        , "g+y"
        , "h+r"
        , "h+w"
        , "k+l"
        , "k+r"
        , "k+w"
        , "k+y"
        , "kh+r"
        , "kh+w"
        , "kh+y"
        , "l+b"
        , "l+c"
        , "l+d"
        , "l+g"
        , "l+h"
        , "l+j"
        , "l+k"
        , "l+ng"
        , "l+p"
        , "l+t"
        , "l+w"
        , "m+r"
        , "m+y"
        , "n+r"
        , "ny+w"
        , "p+r"
        , "p+y"
        , "ph+r"
        , "ph+y"
        , "ph+y+w"
        , "r+b"
        , "r+d"
        , "r+dz"
        , "r+g"
        , "r+g+w"
        , "r+g+y"
        , "r+j"
        , "r+k"
        , "r+k+y"
        , "r+l"
        , "r+m"
        , "r+m+y"
        , "r+n"
        , "r+ng"
        , "r+ny"
        , "r+t"
        , "r+ts"
        , "r+ts+w"
        , "r+w"
        , "s+b"
        , "s+b+r"
        , "s+b+y"
        , "s+d"
        , "s+g"
        , "s+g+r"
        , "s+g+y"
        , "s+k"
        , "s+k+r"
        , "s+k+y"
        , "s+l"
        , "s+m"
        , "s+m+r"
        , "s+m+y"
        , "s+n"
        , "s+n+r"
        , "s+ng"
        , "s+ny"
        , "s+p"
        , "s+p+r"
        , "s+p+y"
        , "s+r"
        , "s+t"
        , "s+ts"
        , "s+w"
        , "sh+r"
        , "sh+w"
        , "t+r"
        , "t+w"
        , "th+r"
        , "ts+w"
        , "tsh+w"
        , "z+l"
        , "z+w"
        , "zh+w"
        ]

-- | The Wylie mnemonic the reference prints for the caret over a lone
-- letter (the jsewts @m_tib_caret@ table): a stack of one letter under a
-- caret spells as that mnemonic (ཕ༹ -> "f", བ༹ -> "v").
caretLetters :: [(Text, Text)]
caretLetters = [("ph", "f"), ("b", "v")]

-- | The suffix consonants: the reference lets the last of several stacks
-- keep the letter it reads as, with no implicit @a@, when the stack is one
-- of these letters (the jsewts @m_suffixes@ table) - the closing @ས@ of
-- བརྙེས reads as "s" alone, while a lone letter still carries the @a@
-- (ས -> "sa", since a lone stack is never a suffix).
suffixLetters :: Set Text
suffixLetters =
    Set.fromList
        [ "'"
        , "\x2018"
        , "\x2019"
        , "g"
        , "ng"
        , "d"
        , "n"
        , "b"
        , "m"
        , "r"
        , "l"
        , "s"
        , "N"
        , "T"
        , "-n"
        , "-t"
        ]

-- | The roots a prefix letter may stand before in the reference's U->W
-- reading (the jsewts @m_prefixes@ table): only there does the prefix lose
-- its implicit @a@. The grammar reads གརླ as prefix + root + subfix, but g is
-- no prefix before @r+l@ and the reference spells it "garla", not "grla".
prefixRoots :: [(Text, Set Text)]
prefixRoots =
    [ ("g", set ["c", "ny", "t", "d", "n", "ts", "zh", "z", "y", "sh", "s"])
    ,
        ( "d"
        , set
            [ "k"
            , "g"
            , "ng"
            , "p"
            , "b"
            , "m"
            , "k+y"
            , "g+y"
            , "p+y"
            , "b+y"
            , "m+y"
            , "k+r"
            , "g+r"
            , "p+r"
            , "b+r"
            ]
        )
    ,
        ( "b"
        , set
            [ "k"
            , "g"
            , "c"
            , "t"
            , "d"
            , "ts"
            , "zh"
            , "z"
            , "sh"
            , "s"
            , "r"
            , "l"
            , "k+y"
            , "g+y"
            , "k+r"
            , "g+r"
            , "r+l"
            , "s+l"
            , "r+k"
            , "r+g"
            , "r+ng"
            , "r+j"
            , "r+ny"
            , "r+t"
            , "r+d"
            , "r+n"
            , "r+ts"
            , "r+dz"
            , "s+k"
            , "s+g"
            , "s+ng"
            , "s+ny"
            , "s+t"
            , "s+d"
            , "s+n"
            , "s+ts"
            , "r+k+y"
            , "r+g+y"
            , "s+k+y"
            , "s+g+y"
            , "s+k+r"
            , "s+g+r"
            , "l+d"
            , "l+t"
            , "k+l"
            , "s+r"
            , "z+l"
            , "s+w"
            ]
        )
    ,
        ( "m"
        , set
            [ "kh"
            , "g"
            , "ng"
            , "ch"
            , "j"
            , "ny"
            , "th"
            , "d"
            , "n"
            , "tsh"
            , "dz"
            , "kh+y"
            , "g+y"
            , "kh+r"
            , "g+r"
            ]
        )
    ,
        ( "'"
        , set
            [ "kh"
            , "g"
            , "ch"
            , "j"
            , "th"
            , "d"
            , "ph"
            , "b"
            , "tsh"
            , "dz"
            , "kh+y"
            , "g+y"
            , "ph+y"
            , "b+y"
            , "kh+r"
            , "g+r"
            , "d+r"
            , "ph+r"
            , "b+r"
            ]
        )
    ]
    where
        set = Set.fromList

-- | The second suffix the reference allows after a first one (the jsewts
-- @m_suff2@ table): @s@ closes after g/ng/b/m, @d@ after n/r/l (དགས ->
-- "dgas", མགས -> "mags").
suff2Before :: [(Text, Set Text)]
suff2Before = [("s", set ["g", "ng", "b", "m"]), ("d", set ["n", "r", "l"])]
    where
        set = Set.fromList

-- | The root of a three-stack syllable that reads as prefix, suffix and second
-- suffix at once (the jsewts @m_ambiguous_key@ table): the middle letter for
-- dgs/dms, the first for mgs/bgs (མགས -> "mags", དགས -> "dgas").
ambiguousRoot :: [(Text, Int)]
ambiguousRoot =
    [ ("dgs", 1)
    , ("dms", 1)
    , ("dngs", 0)
    , ("'gs", 1)
    , ("'bs", 1)
    , ("mngs", 0)
    , ("mgs", 0)
    , ("bgs", 0)
    , ("dbs", 1)
    ]

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
    items <- parseEither (pSentence Tibetan) tokens
    pure (items, diagnostics <> legality items)

-- | The spelling warnings for a whole parsed sentence: the 'syllableDiags' of
-- every syllable run, in source order.
legality :: [SpellItem] -> Diagnostics
legality = foldMap itemDiags
    where
        itemDiags (SyllableItem s) = syllableDiags s
        itemDiags (InvalidSyllableItem s) = syllableDiags s
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
        itemHoldsElement (InvalidSyllableItem _) = True
        itemHoldsElement (Number _) = True
        itemHoldsElement (Punct ts) = any isElement ts
        itemHoldsElement (Other ts) = any isElement ts
        isElement t = case tokenCanonical t of
            TcUnknown _ -> False
            _ -> True

-- | Extract only the recognized syllables from Tibetan text, each as its
-- raw spelling (e.g. @མཆོག་དེ་དུ-> @["མཆོག","དེ"]@).
-- The trailing boundary of each run is skipped.
syllables :: Text -> Either Text [Text]
syllables input = do
    items <- splitSentences input
    pure
        [ T.concat
            [tokenRaw t | (_, t) <- toList (syllableTokens s), not (isBoundaryToken t)]
        | SyllableItem s <- items
        ]
    where
        isBoundaryToken t = case tokenCanonical t of
            TcSpace _ -> True
            TcPunctuation _ -> True
            _ -> False

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

-- | Convert every token to an 'OutputFormat' spelling, preserving
-- everything: same-script tokens keep their exact raw spelling (including
-- aliases), cross-script tokens become canonical representatives, and
-- tokens without a cross-script spelling fall back to their raw spelling.
--
-- The two arms walk the items differently. The Unicode arm prints one item at
-- a time. The Wylie arm walks the items the way the reference walks the
-- characters of its input, because a stretch of foreign text is one block
-- across item boundaries: a run, a number and a sign of punctuation spell in
-- Wylie, and every other character joins the bracket block the reference
-- writes foreign text in (step 5.3).
renderItems :: OutputFormat -> [SpellItem] -> Text
renderItems fmt items = case fmt of
    OutUnicode -> T.concat (map renderItem items)
    OutWylie -> renderForeign (concatMap wyliePieces items)
    where
        renderItem (SyllableItem s) =
            -- The Unicode renderer reads the position marks (wave 3.3): a
            -- subfix prints its subjoined letter, an implicit @a@ prints
            -- nothing, a run-initial vowel gets the a-chen written out.
            renderSyllable (syllableTokens s)
        renderItem (InvalidSyllableItem s) =
            -- A run the grammar read nothing of. Its letters are still
            -- converted one by one: a lone sign vowel is a letter like any
            -- other (ཱ -> A), and nothing the grammar did not recognize may
            -- hide, join or skip a letter. A final mark in such a run is the
            -- exception: no letter claimed it, so there is nothing to convert
            -- it *to* and it stands as it was written (Mi -> M, not ཾ) - the
            -- same thing the reference does with a character that is neither a
            -- letter to it nor special.
            rawSyllable OutUnicode s
        renderItem (Number ts) = T.concat (map (renderToken OutUnicode) ts)
        renderItem (Punct ts) = T.concat (map (renderToken OutUnicode) ts)
        renderItem (Other ts) = T.concat (map (renderToken OutUnicode) ts)

        -- The Wylie pieces of one item: a run spells as a whole (the same
        -- mark-aware walk the Wylie renderer does, wave 3.5), a number and a
        -- sign spell per token - and a token no table reads as Tibetan is a
        -- character of foreign text.
        --
        -- A mark at the head of a run with no letter to attach to never
        -- reaches that walk: the reference escapes such a character
        -- (@needs a top symbol@), so an orphan prefix becomes escapes and
        -- the rest of the run spells as before (wave 5.4).
        --
        -- shortcut: an ASCII space the run's own boundary closes with stays
        -- inside the run and spells @_@, so a space between a run and foreign
        -- text does not join the block behind it (བླ x -> "bla_[x]", the
        -- reference "bla[ x]"); split the trailing spaces off the run when a
        -- corpus needs that shape.
        wyliePieces :: SpellItem -> [Piece]
        wyliePieces (SyllableItem s) =
            let (orphans, rest) = splitOrphans (syllableTokens s)
             in orphanEscapes (toList orphans) <> [runPiece (renderSyllableWylie rest)]
        wyliePieces (InvalidSyllableItem s) =
            let (orphans, rest) = splitOrphans (syllableTokens s)
             in orphanEscapes (toList orphans) <> [runPiece (rawTokens OutWylie (toList rest))]
        wyliePieces (Number ts) = map wyliePiece ts
        wyliePieces (Punct ts) = map wyliePiece ts
        wyliePieces (Other ts) = map wyliePiece ts

        -- A run whose whole spelling is an escape stands for the characters
        -- one by one, so it joins a block like any other escape (x + 0F6B ->
        -- [xཫ]) instead of closing it.
        runPiece txt
            | T.isPrefixOf "\\" txt = Escaped txt
            | otherwise = Plain txt

        -- The orphan prefix of a run: the marks before the first letter, and
        -- everything the run holds behind them.
        splitOrphans toks = Seq.spanl (isToplessMark . snd) toks

        -- One escape per source character of an orphan prefix. The five
        -- forms the reference rewrites into components before reading
        -- anything (jsewts.js:1784-1788) escape component by component, so
        -- they read back into the same glyph; every other orphan escapes as
        -- the character it was written - a continuation of the group carries
        -- no character of its own.
        orphanEscapes :: [(a, Token)] -> [Piece]
        orphanEscapes = go False
            where
                go _ [] = []
                go rewritten ((_, tok) : rest)
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

        rawSyllable ofmt s = rawTokens ofmt (toList (syllableTokens s))

        rawTokens ofmt = T.concat . map (echo ofmt . snd)

        echo ofmt tok
            | isFinalMark tok = tokenRaw tok
            | otherwise = renderToken ofmt tok

        isFinalMark tok = case tokenCanonical tok of
            TcFinal _ -> True
            _ -> False

        -- Whether a Nothing-placed token is a stack separator (@.@ or @+@):
        -- the only unmarked tokens a run may hide or keep.
        isConSpec tok = case tokenCanonical tok of
            TcConSpec _ -> True
            _ -> False

        -- The join sign of Wylie stack syntax: unlike a dot, it has no glyph
        -- of its own, so an unmarked '+' always prints nothing.
        isLoneJoin tok = case tokenCanonical tok of
            TcConSpec CSPlus -> True
            _ -> False

        -- Whether the rest of a run still carries a marked (Just) token.
        anyMarked = any (isJust . fst)

        renderSyllable :: Seq (Maybe Position, Token) -> Text
        renderSyllable ms =
            prependA (toList ms) <> T.concat (go False (toList ms))
            where
                prependA ((Just Vowel, tok) : _)
                    | tokenSource tok == TsWylie = "ཨ"
                prependA _ = ""

                -- A mark a window took and named prints nothing: it is in the
                -- parse and the finding quotes the run, but the reference drops
                -- it from the output too (oMM -> ཨོཾ), and a second caret has
                -- always printed this way (g^r^a -> གྲ༹).
                renderMark (Just Eaten, _) = ""
                -- A Tibetan token always prints its own slice (identity holds even
                -- when a mark would hide or join it: གཨ prints its ཨ, གྲ prints
                -- its joined ྲ).
                renderMark (Just _, tok) | tokenSource tok == TsUnicode = tokenRaw tok
                -- The implicit @a@ a Wylie writer always spells after a base.
                renderMark (Just ImplicitVowel, _) = ""
                renderMark (Just Subfix, tok) = subjoinedGlyph tok
                renderMark (Just _, tok) = renderToken OutUnicode tok
                -- Unmarked tokens never reach the renderer ('go' prints the
                -- unmarked tail and the boundary itself); the clause keeps the
                -- definition total.
                renderMark (Nothing, tok) = renderToken OutUnicode tok

                -- The letters under a superfix print subjoined (rka -> རྐ, sgra ->
                -- སྒྲ): jsewts writes every letter after the superscript in its
                -- subjoined form, so a root below the superfix follows the same
                -- rule - when the stack reaches a vowel. A stack that never does
                -- backtracks, and the next letter is a fresh base instead (rk ->
                -- ར + ཀ). The syllable keeps the marks in the order the grammar
                -- emitted them ('Convert.Sentence' splices the unmarked tokens
                -- back into their source gaps), so a stack-final caret written
                -- mid-stack (g^ra) already prints after the subscripts (གྲ༹).
                go :: Bool -> [(Maybe Position, Token)] -> [Text]
                go _ [] = []
                go underSuperfix (m@(Just Root, tok) : rest)
                    | underSuperfix && runReachesVowel rest = subjoinedGlyph tok : go False rest
                    | otherwise = renderMark m : go False rest
                go _ (m@(Just Superfix, _) : rest) = renderMark m : go True rest
                go _ (m@(Just _, _) : rest) = renderMark m : go False rest
                go _ ((Nothing, tok) : rest)
                    -- A join sign that joins nothing reads as nothing: '+' is
                    -- Wylie-only stack syntax and prints in neither a dead
                    -- tail (g+X -> ག༷, the sign still prints) nor a marked
                    -- one, where the shared clause below already hides it.
                    | isLoneJoin tok = go False rest
                    -- An unmarked token the grammar ate in the middle of a claimed
                    -- stack prints nothing (a stack dot, a doubled caret); only
                    -- the unmarked tail and the boundary print their raw slices.
                    | anyMarked rest = go False rest
                    | otherwise = renderToken OutUnicode tok : go False rest

                -- Whether the marks after a superfix's root reach a vowel before
                -- anything that ends the stack: only subfixes (and a caret) may
                -- stand between the root and the vowel.
                runReachesVowel :: [(Maybe Position, Token)] -> Bool
                runReachesVowel ((pos, tok) : rest) =
                    case pos of
                        Just Subfix -> runReachesVowel rest
                        Just Final
                            | tokenCanonical tok == TcFinal FMCaret -> runReachesVowel rest
                        Just Vowel -> True
                        Just ImplicitVowel -> True
                        _ -> False
                runReachesVowel [] = False

                subjoinedGlyph tok =
                    case subjoinOf tok of
                        Just sc -> fromMaybe "" (unicodeOf (TcSubConsonant sc))
                        Nothing -> renderToken OutUnicode tok

        -- Wylie output takes two shapes, by the script the run was parsed
        -- from. A Wylie-sourced run already spells in Wylie and prints per
        -- token, with three mark-aware additions that keep the spelling
        -- readable back by the Wylie arms:
        --
        --   * the implicit @a@ of a vowel-less bare full root is written out
        --     when the root is followed by another full letter (སའི ->
        --     "sa'i", གནག -> "gnag") - the run's closing letter never
        --     gets it (མངའ -> "mnga'", ས -> "s");
        --   * a full root that follows a prefix or superfix letter gets a dot
        --     before it when the two letters could glue into a subjoined
        --     stack (གཡུལ -> "g.yul", བརས -> "b.ras") - གནག -> "gnag" stays
        --     compact because n never subjoins;
        --   * a run-initial a-chen that only carries a vowel sign is not
        --     written out (ཨུ -> "u"), since the vowel alone restores it.
        --
        -- A subjoined letter prints as its full letter (ླ -> "l"), because the
        -- Wylie tokenizer has no spelling for the Tibetan subjoined glyphs: a
        -- glyph spelled straight into Wylie input stays an unknown token. The
        -- stacks still read back, through the subfix arms of the bare @y, w,
        -- r, l@ and through the superfix arms of the rest; only the forms
        -- above gain separators.
        --
        -- A glyph-sourced run has no spelling to keep yet: the grammar read
        -- positions off Tibetan codepoints, and the stack the spelling needs
        -- is built here (wylieFromGlyphs).
        renderSyllableWylie :: Seq (Maybe Position, Token) -> Text
        renderSyllableWylie marks
            | glyphSourced ms = wylieFromGlyphs ms
            | otherwise = go Nothing ms
            where
                ms = toList marks

                glyphSourced ((_, tok) : _) = tokenSource tok == TsUnicode
                glyphSourced [] = False

                go _ [] = ""
                go prev (m : rest) =
                    markText prev m rest <> implicitA m rest <> go (Just m) rest

                markText _ (Just Root, tok) rest
                    | isAChen tok, nextIsVowel rest = ""
                markText prev (Just Root, tok) _
                    | isFullLetter tok
                    , needsDot prev tok =
                        "." <> renderToken OutWylie tok
                markText _ (Nothing, tok) rest
                    | isConSpec tok, anyMarked rest = ""
                    | otherwise = renderToken OutWylie tok
                markText _ (Just Eaten, _) _ = ""
                markText _ (_, tok) _ = renderToken OutWylie tok

                -- Whether the letters could form a subjoined stack: only then
                -- does a dot between a prefix letter and a following full
                -- root change how the spelling is read.
                needsDot (Just (Just prevPos, prevTok)) tok =
                    isFullLetter prevTok
                        && prevPos `elem` [Prefix, Superfix]
                        && isSubjoinLetter tok
                needsDot _ _ = False

                -- The implicit @a@ after a vowel-less bare full root, written
                -- only before the next full letter (a Root, Suffix, Postfix
                -- or Final mark). A run's closing letter never gets it
                -- (མངའ -> "mnga'", ས -> "s"): a trailing @a@ would read back
                -- as a fresh a-chen once the run already ran its course.
                implicitA (Just Root, tok) rest
                    | isFullLetter tok
                    , not (isAChen tok)
                    , not (isEscapeSpelling tok) =
                        case rest of
                            (Just nextPos, _) : _
                                | nextPos `elem` [Root, Suffix, Postfix, Final] -> "a"
                            _ -> ""
                implicitA _ _ = ""

                isFullLetter tok =
                    case tokenCanonical tok of
                        TcConsonant _ -> True
                        _ -> False

                isAChen tok = tokenCanonical tok == TcConsonant Ca

                isSubjoinLetter tok =
                    case tokenCanonical tok of
                        TcConsonant c -> c `elem` [Cy, Cr, Cl, Cw]
                        _ -> False

                nextIsVowel rest =
                    case rest of
                        (Just Vowel, _) : _ -> True
                        _ -> False

                -- A glyph-sourced run prints stack by stack, the way the
                -- reference joins a Tibetan syllable: a full letter opens a
                -- stack, every subjoined glyph, vowel and final after it joins
                -- that stack, and the stack reads as the reference spells it -
                -- plainly when its letters name a regular stack, with an
                -- explicit '+' before each letter after the first otherwise
                -- (རྒྱ -> "rgya", དྷ -> "d+ha").
                --
                -- Which stack is a prefix, a suffix or a second suffix - and so
                -- which one loses its implicit @a@ - is the reference's own
                -- table reading (7.5), not the grammar's positions: the grammar
                -- reads གརླ as prefix g + root r + subfix l, the reference reads
                -- "ga" + "rla", because g is no prefix before @r+l@ (step 5.7).
                -- A dot separates the prefix from the stack after it where the
                -- two could read as one regular stack, and the reference's
                -- ambiguity table settles three-stack syllables (mgs -> "mags",
                -- dgs -> "dgas").
                wylieFromGlyphs :: [(Maybe Position, Token)] -> Text
                wylieFromGlyphs marks =
                    T.concat (zipWith3 renderStack [0 ..] roles stacks)
                    where
                        stacks = map orderStack (splitStacks marks)
                        (pfxs, sfxs, sf2s) =
                            stackRoles (map stackCons stacks) (map stackSingleCons stacks)
                        roles =
                            [ (pfxs !! i, sfxs !! i, sf2s !! i)
                            | i <- [0 .. length stacks - 1]
                            ]
                        dotAfterFirst = case (stacks, conss) of
                            (first : _, _ : c : _) ->
                                case stackSingleCons first of
                                    Just nm ->
                                        pfxs !! 0
                                            && Set.member (nm <> "+" <> c) regularStacks
                                    Nothing -> False
                            _ -> False
                        conss = map stackCons stacks

                        stackNames st =
                            [renderToken OutWylie (snd m) | m <- st, isLetterTok (snd m)]

                        stackCons = T.intercalate "+" . stackNames

                        -- The reference's @single_cons@: one letter and nothing
                        -- else on the stack (no caret, vowel or final); the
                        -- a-chen is no consonant of its own.
                        stackSingleCons st =
                            case stackNames st of
                                [nm]
                                    | not (any isCaretMark st)
                                    , not (any (isVowelPos . fst) st)
                                    , not (any isLateFinal st)
                                    , nm /= "a" ->
                                        Just nm
                                _ -> Nothing

                        -- The prefix/suffix/second-suffix roles of every stack,
                        -- as the reference computes them over a tsekbar.
                        stackRoles csList singles0 =
                            (prefixes', suffixes', suff2s')
                            where
                                n = length stacks

                                at i xs
                                    | i >= 0 && i < length xs = Just (xs !! i)
                                    | otherwise = Nothing

                                hasPrefix = case (at 0 singles0, at 1 csList) of
                                    (Just (Just nm), Just after) ->
                                        maybe
                                            False
                                            (Set.member (T.replace "+w" "" after))
                                            (lookup nm prefixRoots)
                                    _ -> False

                                lastIsSuffix = case at (n - 1) singles0 of
                                    Just (Just nm) -> Set.member nm suffixLetters
                                    _ -> False

                                secondSuffix =
                                    n > 2 && case (at (n - 1) singles0, at (n - 2) singles0) of
                                        (Just (Just lastNm), Just (Just prevNm)) ->
                                            Set.member prevNm suffixLetters
                                                && maybe
                                                    False
                                                    (Set.member prevNm)
                                                    (lookup lastNm suff2Before)
                                        _ -> False

                                -- two stacks that read as prefix and suffix at
                                -- once: the first is the root (dgas, not d.gas)
                                prefixes =
                                    [ i == 0 && hasPrefix && not (n == 2 && lastIsSuffix)
                                    | i <- [0 .. n - 1]
                                    ]

                                roleSuffixes =
                                    [ n > 1
                                        && ( (i == n - 1 && lastIsSuffix)
                                                || (i == n - 2 && secondSuffix)
                                           )
                                    | i <- [0 .. n - 1]
                                    ]
                                roleSuff2s =
                                    [i == n - 1 && secondSuffix | i <- [0 .. n - 1]]

                                setAt i v xs =
                                    [if j == i then v else x | (j, x) <- zip [0 ..] xs]

                                -- three stacks that read as prefix, suffix and
                                -- second suffix go through the ambiguity table;
                                -- an unknown key falls back to the middle root
                                (prefixes', suffixes', suff2s')
                                    | n == 3
                                    , prefixes !! 0
                                    , roleSuffixes !! 1
                                    , roleSuff2s !! 2 =
                                        let r =
                                                fromMaybe
                                                    1
                                                    ( lookup
                                                        (T.concat (map (fromMaybe "") singles0))
                                                        ambiguousRoot
                                                    )
                                         in ( setAt r False prefixes
                                            , setAt r False roleSuffixes
                                            , setAt (r + 1) False roleSuff2s
                                            )
                                    | otherwise = (prefixes, roleSuffixes, roleSuff2s)

                        renderStack i (isPrefixRole, isSuffixRole, isSuff2Role) st =
                            case st of
                                ((_, tok) : _)
                                    | isFullTok tok ->
                                        chenDot
                                            <> lettersTxt
                                            <> caretTxt
                                            <> vowelTxt
                                            <> finalsTxt
                                            <> othersTxt
                                            <> prefixDot
                                    where
                                        names = stackNames st
                                        cons = T.intercalate "+" names
                                        firstTok = tok
                                        vowels =
                                            [ v
                                            | m <- st
                                            , isVowelPos (fst m)
                                            , Just v <- [vowelOf (snd m)]
                                            ]
                                        hasCaret = any isCaretMark st
                                        folded = case names of
                                            [nm] | hasCaret -> lookup nm caretLetters
                                            _ -> Nothing
                                        -- an a-chen that only carries a vowel
                                        -- prints as the vowel alone (ཨུ -> "u")
                                        dropAChen =
                                            isAChen firstTok
                                                && length names == 1
                                                && not (null vowels)
                                        lettersTxt = case folded of
                                            Just l -> l
                                            Nothing
                                                | dropAChen -> ""
                                                | Set.member cons regularStacks -> T.concat names
                                                | otherwise -> cons
                                        caretTxt = case folded of
                                            Just _ -> ""
                                            Nothing
                                                | hasCaret -> "^"
                                                | otherwise -> ""
                                        vowelTxt = case vowels of
                                            [] -> implicitTxt
                                            VA : v : rest
                                                | Just combined <- longVowel v ->
                                                    T.intercalate
                                                        "+"
                                                        (combined : map vowelName rest)
                                            vs -> T.intercalate "+" (map vowelName vs)
                                        implicitTxt
                                            | isPrefixRole || isSuffixRole || isSuff2Role = ""
                                            | T.isSuffixOf "a" cons = ""
                                            -- an escape spelling stands for the
                                            -- whole character, inherent vowel
                                            -- and all (@\u0f6b@ is ཫ)
                                            | any (isEscapeSpelling . snd) letterMs = ""
                                            | otherwise = "a"
                                        letterMs = [m | m <- st, isLetterTok (snd m)]
                                        finalsTxt =
                                            T.concat
                                                [ renderToken OutWylie (snd m)
                                                | m <- st
                                                , isLateFinal m
                                                ]
                                        othersTxt =
                                            T.concat
                                                [ renderToken OutWylie (snd m)
                                                | m <- st
                                                , isStrayMark m
                                                ]
                                        chenDot
                                            | isAChen firstTok, any isLetterStack (take i stacks) = "."
                                            | otherwise = ""
                                        prefixDot
                                            | i == 0, dotAfterFirst = "."
                                            | otherwise = ""
                                        isStrayMark m =
                                            not (isStackMark m)
                                                && not (isEatenMark m)
                                                && not (isHiddenJoin m)
                                        isEatenMark (Just Eaten, _) = True
                                        isEatenMark _ = False
                                        isHiddenJoin (Nothing, t) = isConSpec t
                                        isHiddenJoin _ = False
                                _ -> T.concat (map loneMark st)

                        loneMark m = case m of
                            (Just Eaten, _) -> ""
                            (Nothing, tok) | isConSpec tok -> ""
                            (_, tok) -> renderToken OutWylie tok

                        isLetterStack st = case st of
                            (_, tok) : _ -> isFullTok tok
                            [] -> False

                        vowelOf tok = case tokenCanonical tok of
                            TcVowel v -> Just v
                            _ -> Nothing

                        vowelName v = fromMaybe "" (wylieOf (TcVowel v))

                        -- A long vowel: the @ཱ@ sign and the short sign that
                        -- completes it are one mnemonic in Wylie (@I@ @U@ @-I@),
                        -- so the pair prints as one vowel.
                        longVowel :: Vowel -> Maybe Text
                        longVowel Vi = Just "I"
                        longVowel Vu = Just "U"
                        longVowel V_i = Just "-I"
                        longVowel _ = Nothing

                -- The reference reads a stack's marks into separate lists -
                -- letters, caret, vowels, finals - and prints them in that
                -- order, so a mark written before a later one still prints
                -- after it: a final before a subjoined letter (བཾྲ -> "braM")
                -- or before a vowel (བཾུ -> "buM"), a vowel sign before a
                -- subjoined letter (བོྲ -> "bro"). The split is at a full
                -- letter, which opens the next stack.
                splitStacks [] = []
                splitStacks (m : rest)
                    | isFullTok (snd m) =
                        let (segment, after) = break (isFullTok . snd) rest
                         in (m : segment) : splitStacks after
                    | otherwise = [m] : splitStacks rest

                -- A stable partition, so the letters keep the order they were
                -- written in; an eaten mark prints nothing and goes last.
                orderStack stackMarks =
                    [m | m <- stackMarks, isLetterTok (snd m)]
                        <> [m | m <- stackMarks, isCaretMark m]
                        <> [m | m <- stackMarks, isVowelPos (fst m)]
                        <> [m | m <- stackMarks, isLateFinal m]
                        <> [m | m <- stackMarks, not (isStackMark m)]

                isCaretMark (Just Final, tok) = tokenCanonical tok == TcFinal FMCaret
                isCaretMark _ = False
                isLateFinal (Just Final, tok) = tokenCanonical tok /= TcFinal FMCaret
                isLateFinal _ = False
                isStackMark m =
                    isLetterTok (snd m)
                        || isCaretMark m
                        || isVowelPos (fst m)
                        || isLateFinal m

                isLetterTok tok = case tokenCanonical tok of
                    TcConsonant _ -> True
                    TcSubConsonant _ -> True
                    _ -> False
                isFullTok tok = case tokenCanonical tok of
                    TcConsonant _ -> True
                    _ -> False
                isVowelPos pos = case pos of
                    Just Vowel -> True
                    Just ImplicitVowel -> True
                    _ -> False

        renderToken ofmt tok
            | tokenSource tok == sourceOf ofmt = tokenRaw tok
            -- a token without a cross-script spelling falls back to the text it
            -- stands for: the content of a bracket block, the character of an
            -- escape, and otherwise the raw slice
            | otherwise = fromMaybe (unknownText tok) (scriptOf ofmt (tokenCanonical tok))

        unknownText tok =
            case tokenCanonical tok of
                TcUnknown (UnknownMark text) -> text
                _ -> tokenRaw tok

        sourceOf OutUnicode = TsUnicode
        sourceOf OutWylie = TsWylie

        scriptOf OutUnicode = unicodeOf
        scriptOf OutWylie = wylieOf
