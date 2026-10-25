-- | The Wylie output invariants of REFACTOR-AST §9.1: the notation (item 5)
-- and W→W (item 4). Both are corpus runs: the reference supplies the cases,
-- the property is ours, so the group sits outside @reference vectors@.
--
-- The notation is checked on what it promises: the same vertical units with
-- the same signs. A role is not part of the promise — the grammar reads it off
-- the writing, and the reference's own @b.ras@ for བརས reads back as root b,
-- root r, suffix s although it was written from a prefix. The implicit @a@ is
-- not compared either (§9.1 item 3), and an a-chen that only carries a written
-- vowel is not written at all (§5: ཨུ -> @u@), so its root letter is left out.
--
-- Two drifts the corpora reproduce are recorded below instead of fixed: the
-- plan makes a drift a defect of @+@/@.@ to journal first and decide later
-- (§6.1), and neither fix is a local one. The check fails on a new drift and
-- on a recorded one that closes, so the list cannot rot.
module Test.Wylie.Output (tests) where

import Convert (OutputFormat (..), SpellItem (..), pSentence, renderItems)
import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Syllable
    ( Stack (..)
    , Subfixes (..)
    , Syllable (..)
    , VowelSlot (..)
    , finalsTokens
    )
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (..)
    , tokenCanonical
    )
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie, wylieOf)
import Convert.Wylie.Tables (Join (..), notate, solidStacks)
import Data.Foldable (toList)
import Data.List (nub, sort, (\\))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Corpus (Row (..), corpora, kangyurText, readRows)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "wylie output"
        [ bookStacks
        , outsideTheBook
        , testCase "corpora: parse -> notate -> parse keeps the stacks" notationCorpus
        , testCase "corpora: W->W keeps the glyphs" w2wCorpus
        ]

-- | REFACTOR-AST §9.1 item 5, first half: every entry of the book table,
-- written solid with the implicit @a@, is one stack and notates back to the
-- same solid writing (198 entries today).
bookStacks :: TestTree
bookStacks = testCase "every book stack reads back solid" $ do
    let broken = mapMaybe check (Set.toList solidStacks)
    assertBool
        ("book stack does not read back solid:\n" <> unlines broken)
        (null broken)
    where
        check key =
            let solid = T.filter (/= '+') key <> "a"
                label = T.unpack key <> " (" <> T.unpack solid <> ")"
             in case syllables (parseWylie solid) of
                    [s] -> case syllables (parseWylie (notation s)) of
                        [back]
                            | notation s == solid, content back == content s -> Nothing
                            | otherwise ->
                                Just
                                    ( label
                                        <> ": notates "
                                        <> T.unpack (notation s)
                                        <> ", reads back as "
                                        <> T.unpack (showContent back)
                                    )
                        _ -> Just (label <> ": notates into more than one syllable")
                    _ -> Just (label <> ": is not one stack")

-- | REFACTOR-AST §9.1 item 5, second half: a combination outside the book is
-- written with @+@ and reads back; a boundary the parse parts by itself needs
-- no dot. @s+l+k@ is no book run (4.9 takes @s+l@ and the second letter is
-- only @y w r@), @grla@ is the boundary control of §6.1.
outsideTheBook :: TestTree
outsideTheBook =
    testGroup
        "a combination outside the book"
        [ testCase "s+l+ka takes + and reads back" $ do
            s <- lone "s+l+ka"
            notation s @?= "s+l+ka"
            (showContent <$> syllables (parseWylie (notation s))) @?= [showContent s]
        , testCase "grla needs no dot: the parse parts it by itself" $ do
            s <- lone "grla"
            notation s @?= "grla"
            (showContent <$> syllables (parseWylie (notation s))) @?= [showContent s]
        ]
    where
        lone input = case syllables (parseWylie input) of
            [s] -> pure s
            other ->
                assertFailure
                    ("expected one syllable for " <> show input <> ", got " <> show (length other))

-- | REFACTOR-AST §9.1 item 5, corpus half: @parse -> notate -> parse@ on every
-- syllabic run of the five vectors and the Kangyur.
notationCorpus :: Assertion
notationCorpus = do
    cases <- corpusCases
    assertDrifts
        "notation"
        (nub (mapMaybe driftOf cases))
        [("gr+y+wa", "g+r+y+w")]

-- | A notation drift: the writing it produced and the units it reads back as.
driftOf :: Syllable -> Maybe (Text, Text)
driftOf s
    | back == content s = Nothing
    | otherwise = Just (notation s, T.intercalate " | " back)
    where
        back = concatMap content (syllables (parseWylie (notation s)))

-- | REFACTOR-AST §9.1 item 4: a Wylie run written Wylie again keeps the glyphs
-- it had after W→U. Separators may normalize (§8 B2), glyphs may not.
w2wCorpus :: Assertion
w2wCorpus = do
    rows <- concat <$> mapM (readRows . snd) corpora
    assertDrifts
        "W->W"
        (nub (mapMaybe driftOfRow rows))
        [ ("grywa", "gr+y+wa")
        , ("g....yag", "gyag")
        ]
    where
        driftOfRow row
            | renderItems OutUnicode (parseWylie wylie)
                /= renderItems OutUnicode (parseWylie (rowWylie row)) =
                Just (rowWylie row, wylie)
            | otherwise = Nothing
            where
                wylie = renderItems OutWylie (parseWylie (rowWylie row))

-- | The exact set of known drifts, in both directions.
assertDrifts :: String -> [(Text, Text)] -> [(Text, Text)] -> Assertion
assertDrifts what seen recorded = do
    let new = sort seen \\ sort recorded
        closed = sort recorded \\ sort seen
        show1 (input, output) = "  " <> T.unpack input <> "  ->  " <> T.unpack output
    assertBool
        ( unlines
            [ what
                <> " drift appeared; fix it or record it with the reason (REFACTOR-AST §6.1, §11):"
            , unlines (map show1 new)
            ]
        )
        (null new)
    assertBool
        ( unlines
            [ what <> " drift closed; remove the row from the list:"
            , unlines (map show1 closed)
            ]
        )
        (null closed)

-- | Every syllabic run of the corpora, as the syllable the grammar built.
corpusCases :: IO [Syllable]
corpusCases = do
    vectors <- concat <$> mapM vectorCases corpora
    kang <- kangyurText
    pure
        ( vectors
            <> concatMap
                (syllables . parseUnicode)
                (filter (not . T.null) (T.lines kang))
        )
    where
        vectorCases (_, path) = do
            rows <- readRows path
            pure
                ( concatMap (syllables . parseWylie . rowWylie) rows
                    <> concatMap (syllables . parseUnicode . rowUnicode) rows
                )

-- | The notation as the Wylie render writes it: the joins and the signs.
notation :: Syllable -> Text
notation = T.concat . map piece . toList . notate
    where
        piece (JSolid, txt) = txt
        piece (JPlus, txt) = "+" <> txt
        piece (JDot, txt) = "." <> txt

-- | What the notation promises to keep: the vertical units and the signs in
-- them, roles and the implicit @a@ left out (see the header).
content :: Syllable -> [Text]
content = map stackContent . toList . sylStacks

stackContent :: Stack -> Text
stackContent st =
    T.intercalate "+" [mnemonic tok | tok <- letters, Just tok /= unwritten]
        <> caret
        <> vowel
        <> finals
    where
        letters =
            maybe [] pure (stSuperfix st)
                <> maybe [] pure (stRoot st)
                <> subfixTokens (stSubfixes st)
        -- An a-chen that is the whole stack and carries a written vowel is not
        -- written at all: the vowel alone restores it (ཨི -> i). With a subjoined
        -- letter below it the letter must stay, or the stack would be another one.
        unwritten
            | stSuperfix st == Nothing
            , NoSubfix <- stSubfixes st
            , Just (VowelWritten _ _) <- stVowel st
            , Just root <- stRoot st
            , tokenCanonical root == TcConsonant Ca =
                Just root
            | otherwise = Nothing
        caret = maybe "" (const "^") (stCaret st)
        vowel = case stVowel st of
            Nothing -> ""
            Just VowelImplicit -> ""
            Just (VowelWritten tok rest) ->
                ":" <> T.intercalate "+" (map mnemonic (tok : rest))
        finals = case finalsTokens (stFinals st) of
            [] -> ""
            ts -> "#" <> T.concat (map mnemonic ts)

showContent :: Syllable -> Text
showContent = T.intercalate " | " . content

mnemonic :: Token -> Text
mnemonic tok = fromMaybe "" (wylieOf (tokenCanonical tok))

subfixTokens :: Subfixes -> [Token]
subfixTokens NoSubfix = []
subfixTokens (OneSubfix a) = [a]
subfixTokens (TwoSubfix a b) = [a, b]
subfixTokens (ThreeSubfix a b c) = [a, b, c]

syllables :: [SpellItem] -> [Syllable]
syllables = mapMaybe item
    where
        item (SyllableItem s) = Just s
        item _ = Nothing

parseWylie :: Text -> [SpellItem]
parseWylie = parseWith tokenizeWylie

parseUnicode :: Text -> [SpellItem]
parseUnicode = parseWith tokenizeUnicode

parseWith :: (Text -> ([Token], Diagnostics)) -> Text -> [SpellItem]
parseWith tokenizer input =
    case parseEither pSentence (fst (tokenizer input)) of
        Left err ->
            error ("Test.Wylie.Output: cannot parse " <> show input <> ": " <> T.unpack err)
        Right items -> items
