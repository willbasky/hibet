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
    , Span (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , UnknownMark (..)
    , Vowel (VA, V_i, Vi, Vu)
    , mkSpan
    , subjoinOf
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie (wylieOf)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

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
-- or a character of foreign text ('Foreign', the block's own).
data Piece = Plain Text | Space Text | Foreign Text

-- | The reference's foreign-text blocks (@toWylie@ in escaping mode): a
-- maximal stretch of foreign characters prints between brackets, the brackets
-- inside it escaped (@\[with embedded\[\]\]@). An ASCII space joins such a
-- block only where the block already holds foreign text - that is what keeps
-- the leading spaces of "  2tix!" inside one block - and prints as @_@ where
-- nothing foreign follows it.
renderForeign :: [Piece] -> Text
renderForeign [] = ""
renderForeign (p : rest)
    -- every branch must consume at least this head, or the walk repeats it
    | not (bracketable p) = openText p <> renderForeign rest
    | any isForeign block = openBlock block <> renderForeign afterBlock
    | otherwise = T.concat (map openText block) <> renderForeign afterBlock
    where
        (more, afterBlock) = span bracketable rest
        block = p : more
        bracketable (Plain _) = False
        bracketable _ = True
        isForeign (Foreign _) = True
        isForeign _ = False
        -- inside a block every character prints as itself
        blockText (Plain t) = t
        blockText (Space t) = t
        blockText (Foreign t) = t
        openBlock pieces = "[" <> escapeBrackets (T.concat (map blockText pieces)) <> "]"
        escapeBrackets = T.replace "]" "\\]" . T.replace "[" "\\["
        -- outside a block an ASCII space is the reference's own spelling of it
        openText (Space _) = "_"
        openText piece = blockText piece

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
        -- shortcut: an ASCII space the run's own boundary closes with stays
        -- inside the run and spells @_@, so a space between a run and foreign
        -- text does not join the block behind it (བླ x -> "bla_[x]", the
        -- reference "bla[ x]"); split the trailing spaces off the run when a
        -- corpus needs that shape.
        wyliePieces :: SpellItem -> [Piece]
        wyliePieces (SyllableItem s) = [Plain (renderSyllableWylie (syllableTokens s))]
        wyliePieces (InvalidSyllableItem s) = [Plain (rawSyllable OutWylie s)]
        wyliePieces (Number ts) = map wyliePiece ts
        wyliePieces (Punct ts) = map wyliePiece ts
        wyliePieces (Other ts) = map wyliePiece ts

        wyliePiece tok
            | isAsciiSpace tok = Space (tokenRaw tok)
            | isForeignChar tok = Foreign (renderToken OutWylie tok)
            | otherwise = Plain (renderToken OutWylie tok)

        isAsciiSpace tok = tokenCanonical tok == TcSpace SMSpace

        -- Foreign text is a character no table reads as Tibetan. A line break
        -- is not one: the reference ends a block at it and prints it as
        -- written.
        isForeignChar tok = case tokenCanonical tok of
            TcUnknown (UnknownMark text) -> not (T.isInfixOf "\n" text) && not (T.isInfixOf "\r" text)
            _ -> False

        rawSyllable fmt s = T.concat (map (echo fmt . snd) (toList (syllableTokens s)))

        echo fmt tok
            | isFinalMark tok = tokenRaw tok
            | otherwise = renderToken fmt tok

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

                -- Whether a token's Wylie spelling is really an escape
                -- (@\uXXXX@). EWTS has no mnemonic for some characters (the
                -- Balti ཫ ཬ, the half-numbers, the signs), so they spell as an
                -- escape that already stands for the whole character; the
                -- implicit @a@ must not follow it (@\u0f6b@ reads back as ཫ,
                -- @\u0f6ba@ as ཫ + ཨ).
                isEscapeSpelling tok = T.isPrefixOf "\\" (renderToken OutWylie tok)

                isAChen tok = tokenCanonical tok == TcConsonant Ca

                isSubjoinLetter tok =
                    case tokenCanonical tok of
                        TcConsonant c -> c `elem` [Cy, Cr, Cl, Cw]
                        _ -> False

                nextIsVowel rest =
                    case rest of
                        (Just Vowel, _) : _ -> True
                        _ -> False

                -- A glyph-sourced run prints through stacks, the way the
                -- reference joins a Tibetan syllable: a full letter opens a
                -- stack and every subjoined glyph after it joins that stack
                -- (བརྙ is [b] + [r, ny], བྲྙ is [b, r, ny]), so the run
                -- reads as the reference spells it - plainly when the letters
                -- joined by '+' name a regular stack, with an explicit '+'
                -- before each of them otherwise (རྒྱ -> "rgya",
                -- དྷ -> "d+ha"), the dot between a prefix and the stack after
                -- it only where the two could read as one regular stack, and
                -- the implicit @a@ of a vowel-less root stack - even a lone
                -- letter carries it (ས -> "sa", མངའ -> "mnga'").
                wylieFromGlyphs :: [(Maybe Position, Token)] -> Text
                wylieFromGlyphs = goGlyph False Nothing . groupPieces . floatLateSubs

                goGlyph before prevMark (piece : rest) =
                    case longVowelRun before piece rest of
                        Just (combined, rest') ->
                            combined
                                <> goGlyph (before || isLetterPiece piece) (lastMark piece) rest'
                        Nothing ->
                            emitGlyph before prevMark piece rest
                                <> goGlyph (before || isLetterPiece piece) (lastMark piece) rest
                goGlyph _ _ [] = ""

                emitGlyph _ _ [] _ = ""
                emitGlyph before prevMark piece@(m : _) rest
                    | isVowelPiece piece = renderToken OutWylie (snd m)
                    | isLetterPiece piece =
                        stackGlyph
                            before
                            (any isLetterPiece rest)
                            prevMark
                            piece
                            (vowelOwns rest)
                    | otherwise = loneGlyph (anyMarked (concat rest)) m

                -- One stack: its letters, the caret the run put after them,
                -- and the implicit @a@ unless a vowel sign follows or the
                -- spelling already ends in @a@.
                stackGlyph _ _ _ [] _ = ""
                stackGlyph before after prevMark piece@(firstM : _) vowelOver =
                    dot <> letters <> caret <> writtenA
                    where
                        letterMs = filter (isLetterTok . snd) piece
                        -- the caller opens a stack only where the first mark
                        -- is a letter
                        firstTok = snd firstM
                        firstPos = fst firstM
                        names = map (renderToken OutWylie . snd) letterMs
                        plus = T.intercalate "+" names
                        spelled
                            | Set.member plus regularStacks = T.concat names
                            | otherwise = plus
                        -- a lone a-chen under a vowel sign prints as the vowel
                        -- alone: the sign restores the letter (ཨུ -> "u")
                        dropChen =
                            vowelOver
                                && isAChen firstTok
                                && length letterMs == 1
                        letters = case folded of
                            Just l -> l
                            Nothing -> if dropChen then "" else spelled
                        hasCaret = any (\m -> tokenCanonical (snd m) == TcFinal FMCaret) piece
                        caret
                            | isJust folded = ""
                            | hasCaret = "^"
                            | otherwise = ""
                        -- the reference folds the caret over a lone letter
                        -- into one mnemonic (pha + caret -> f, ba + caret ->
                        -- v, m_tib_caret): only a stack of one letter folds
                        folded
                            | hasCaret, [n] <- names, Just l <- lookup n caretLetters = Just l
                            | otherwise = Nothing
                        -- the dot only where the prefix and the stack could
                        -- read as one regular stack: b + r + ny is no regular
                        -- stack, so བརྙ stays "brnya"
                        --
                        -- a stack opening on an a-chen takes a dot of its own
                        -- whenever an earlier stack stands in the run: a bare
                        -- @a@ behind another letter would read back as a single
                        -- stack once the run already runs, so the reference
                        -- separates them (a.a, ba.a, be.a) and no dot is the
                        -- a-chen's own opening stack (aka, gaka, bka)
                        dot
                            | isAChen firstTok, before = "."
                            | otherwise = case prevMark of
                                Just (Just Prefix, prevTok)
                                    | isFullLetter prevTok
                                    , isFullLetter firstTok
                                    , Set.member
                                        (renderToken OutWylie prevTok <> "+" <> plus)
                                        regularStacks ->
                                        "."
                                _ -> ""
                        writtenA
                            | vowelOver = ""
                            | firstPos `notElem` [Just Root, Just Superfix] = ""
                            | T.isSuffixOf "a" plus = ""
                            -- an escape spelling already stands for the whole
                            -- character, inherent vowel and all, so it takes no
                            -- written @a@ (@\u0f6b@ is ཫ, @\u0f6ba@ is ཫ + ཨ)
                            | isEscapeSpelling firstTok = ""
                            -- a lone suffix consonant closing several stacks
                            -- keeps the letter it reads as, with no @a@
                            -- (བརྙེས -> "b+r+nyes", མངའ -> "mnga'")
                            | loneFinalSuffix = ""
                            | otherwise = "a"
                        -- the reference's suffix flag: the last stack of
                        -- several, one letter, and that letter a suffix; a
                        -- lone stack is never one, so a lone letter carries
                        -- the @a@ it stands for (ས -> "sa", ὠ -> "'a")
                        loneFinalSuffix = before && not after && isSuffixName names
                        isSuffixName [n] = Set.member n suffixLetters
                        isSuffixName _ = False

                -- A mark outside every stack: a lone vowel sign already
                -- handled above, a final sign, an eaten mark, a mark the run
                -- left unmarked. It prints per token, as the flat spelling
                -- would.
                loneGlyph afterMarked m = case m of
                    (Just Eaten, _) -> ""
                    (Nothing, tok)
                        | isConSpec tok, afterMarked -> ""
                        | otherwise -> renderToken OutWylie tok
                    (_, tok) -> renderToken OutWylie tok

                -- Group the run into pieces: a stack (a full letter and the
                -- glyphs under it) and the lone marks between the stacks.
                groupPieces :: [(Maybe Position, Token)] -> [[(Maybe Position, Token)]]
                groupPieces = map reverse . finish . foldl step ([], [])
                    where
                        step (cur, done) m
                            | isFullTok tok, not (null cur) = ([m], cur : done)
                            | isFullTok tok = ([m], done)
                            | isSubTok tok, not (null cur) = (m : cur, done)
                            | isSubTok tok = ([m], done)
                            | not (null cur), joinsStack m = (m : cur, done)
                            | not (null cur) = ([], [m] : cur : done)
                            | otherwise = ([], [m] : done)
                            where
                                tok = snd m
                        finish (cur, done) = reverse (if null cur then done else cur : done)
                        joinsStack m = case m of
                            (Just Eaten, _) -> True
                            (Just Final, tok) -> tokenCanonical tok == TcFinal FMCaret
                            _ -> False

                -- A vowel sign that stands between its base and a subjoined
                -- letter (བོྲ) still belongs to the stack the base opened: the
                -- reference reads the syllable as the br-stack plus the vowel
                -- ("bro"), so the sign is floated after the letters it prints
                -- after, where the group builder expects it. The usual order
                -- (base, sub, vowel) passes through untouched.
                floatLateSubs :: [(Maybe Position, Token)] -> [(Maybe Position, Token)]
                floatLateSubs (v : s : rest)
                    | isVowelPos (fst v), isSubTok (snd s) = s : v : floatLateSubs rest
                floatLateSubs (m : rest) = m : floatLateSubs rest
                floatLateSubs [] = []

                isLetterTok tok = case tokenCanonical tok of
                    TcConsonant _ -> True
                    TcSubConsonant _ -> True
                    _ -> False
                isFullTok tok = case tokenCanonical tok of
                    TcConsonant _ -> True
                    _ -> False
                isSubTok tok = case tokenCanonical tok of
                    TcSubConsonant _ -> True
                    _ -> False
                isLetterPiece (m : _) = isLetterTok (snd m)
                isLetterPiece [] = False
                isVowelPiece (m : _) = isVowelPos (fst m)
                isVowelPiece [] = False
                isVowelPos pos = case pos of
                    Just Vowel -> True
                    Just ImplicitVowel -> True
                    _ -> False
                lastMark piece = case reverse piece of
                    m : _ -> Just m
                    [] -> Nothing
                -- whether the next piece is the vowel sign over this stack
                vowelOwns rest = case rest of
                    piece : _ -> isVowelPiece piece
                    _ -> False

                -- The long letter the Tibetan side spells with the @ཱ@ sign and
                -- a short sign: the pair is one mnemonic in Wylie (@I@ @U@ @-I@),
                -- so it prints as one piece and the short sign is skipped. The
                -- mnemonic needs a letter to sit on («I» is ཨཱི, not a bare ཱི),
                -- so the pair combines only after a letter piece of this run
                -- (@before@): «ཱ» + «ི» with no carrier reads @A@ then @i@, as
                -- before. The other short signs that may follow @ཱ@ (@ེ@ @ོ@) do
                -- not combine either: the reference reads «ཱ» + «ེ» as @A@ then
                -- @e@.
                longVowelRun before piece (next : rest)
                    | before
                    , Just VA <- vowelPieceCanon piece
                    , Just v <- vowelPieceCanon next
                    , Just combined <- longVowelCombined v =
                        Just (combined, rest)
                longVowelRun _ _ _ = Nothing

                vowelPieceCanon (m : _)
                    | isVowelPos (fst m) = case tokenCanonical (snd m) of
                        TcVowel v -> Just v
                        _ -> Nothing
                vowelPieceCanon _ = Nothing

                longVowelCombined Vi = Just "I"
                longVowelCombined Vu = Just "U"
                longVowelCombined V_i = Just "-I"
                longVowelCombined _ = Nothing

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
