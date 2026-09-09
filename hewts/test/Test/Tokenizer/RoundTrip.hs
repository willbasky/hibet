module Test.Tokenizer.RoundTrip (tests) where

import Control.Monad (when)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

import Convert (OutputFormat (..), SpellItem (..), pSentence, renderItems, splitSentences)
import Convert.Grammar.Parser (parseEither)
import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode, unicodeOf)
import Convert.Tokenizer.Wylie
    ( tokenizeWylie
    , wylieConsonant
    , wylieConsonantAliases
    , wylieFinal
    , wylieFinalAliases
    , wylieHalfNumber
    , wylieNumber
    , wylieOrnament
    , wyliePunctuation
    , wylieSanskritMark
    , wylieSign
    , wylieSpace
    , wylieSymbol
    , wylieVowel
    , wylieVowelAliases
    )

tests :: TestTree
tests =
    testGroup
        "round-trip"
        [ unicodeToWylieToUnicode
        , wylieToUnicodeToWylie
        , asciiPurity
        , escapeSpellings
        , corpus
        , subjoined
        ]

-- | Parse Tibetan text as Unicode and run the token-level grammar.
parseU :: Text -> [SpellItem]
parseU input =
    case splitSentences input of
        Left err -> error ("RoundTrip.parseU: " <> show err)
        Right items -> items

-- | Parse Wylie text and run the token-level grammar.
parseW :: Text -> [SpellItem]
parseW input =
    case parseEither pSentence (tokenizeWylie input) of
        Left err -> error ("RoundTrip.parseW: " <> show err)
        Right items -> items

renderW :: [SpellItem] -> Text
renderW = renderItems OutWylie

renderU :: [SpellItem] -> Text
renderU = renderItems OutUnicode

-- | Full pipeline: Unicode -> Wylie -> Unicode.
rtU :: Text -> Text
rtU input = renderU (parseW (renderW (parseU input)))

-- | Full pipeline: Wylie -> Unicode -> Wylie.
rtW :: Text -> Text
rtW input = renderW (parseU (renderU (parseW input)))

-- | Every canonical with both a Unicode spelling and a Wylie spelling.
-- @SubConsonant@ is handled separately (see the 'subjoined' group) because
-- its Wylie behaviour depends on a still-open design decision.
canonicals :: [TokenCanonical]
canonicals =
    [ TcConsonant c | c <- [minBound .. maxBound :: Consonant] ]
        <> [ TcVowel v | v <- [minBound .. maxBound :: Vowel] ]
        <> [ TcFinal f | f <- [minBound .. maxBound :: FinalMark] ]
        <> [ TcNumber n | n <- [minBound .. maxBound :: Number] ]
        <> [ TcHalfNumber h | h <- [minBound .. maxBound :: HalfNumber] ]
        <> [ TcPunctuation p | p <- [minBound .. maxBound :: PunctuationMark] ]
        <> [ TcSign s | s <- [minBound .. maxBound :: SignMark] ]
        <> [ TcSanskritMark m | m <- [minBound .. maxBound :: SanskritMark] ]
        <> [ TcOrnament o | o <- [minBound .. maxBound :: OrnamentMark] ]
        <> [ TcSpace m | m <- [minBound .. maxBound :: SpaceMark] ]
        <> [ TcSymbol s | s <- [minBound .. maxBound :: SymbolMark] ]

unicodeToWylieToUnicode :: TestTree
unicodeToWylieToUnicode =
    testGroup
        "unicode -> wylie -> unicode is identity"
        [ testCase (show c) $
            let u =
                    case unicodeOf c of
                        Just x -> x
                        Nothing -> error ("no unicode spelling for " <> show c)
             in checkUnicodeIdentity u (show c)
        | c <- canonicals
        ]

checkUnicodeIdentity :: Text -> String -> Assertion
checkUnicodeIdentity input label =
    let w = renderW (parseU input)
        back = renderU (parseW w)
        msg =
            unwords
                [ "unicode identity failed:"
                , label
                , "in=" <> T.unpack input
                , "w=" <> T.unpack w
                , "back=" <> T.unpack back
                ]
     in assertBool msg (back == input)

-- | Every Wylie spelling the tokenizer accepts, with @True@ when the spelling
-- is an alias that normalizes to a canonical representative on the first pass.
wylieSpellings :: [(Text, Bool)]
wylieSpellings = HS.toList . HS.fromList $ reps' <> aliasKeys
  where
    reps' = [(s, False) | s <- wylieReps]
    aliasKeys = [(s, True) | s <- wylieAliasKeys]

wylieReps :: [Text]
wylieReps =
    [ wylieConsonant c | c <- [minBound .. maxBound :: Consonant] ]
        <> [ wylieVowel v | v <- [minBound .. maxBound :: Vowel] ]
        <> [ wylieFinal f | f <- [minBound .. maxBound :: FinalMark] ]
        <> [ wylieNumber n | n <- [minBound .. maxBound :: Number] ]
        <> [ wylieHalfNumber h | h <- [minBound .. maxBound :: HalfNumber] ]
        <> [ wyliePunctuation p | p <- [minBound .. maxBound :: PunctuationMark] ]
        <> [ wylieSign s | s <- [minBound .. maxBound :: SignMark] ]
        <> [ wylieSanskritMark m | m <- [minBound .. maxBound :: SanskritMark] ]
        <> [ wylieOrnament o | o <- [minBound .. maxBound :: OrnamentMark] ]
        <> [ wylieSymbol s | s <- [minBound .. maxBound :: SymbolMark] ]
        <> [ wylieSpace m | m <- [minBound .. maxBound :: SpaceMark] ]

wylieAliasKeys :: [Text]
wylieAliasKeys =
    map fst wylieConsonantAliases
        <> map fst wylieVowelAliases
        <> map fst wylieFinalAliases

-- | EWTS-only letters whose Unicode spelling decompresses into two separate
-- tokens (base consonant + caret). They stay idempotent but their Wylie form
-- changes after a pass through Unicode, so they are not rep fixed points.
composedWylie :: [String]
composedWylie = ["f", "v"]

wylieToUnicodeToWylie :: TestTree
wylieToUnicodeToWylie =
    testGroup
        "wylie -> unicode -> wylie is canonical-idempotent"
        [ testCase (T.unpack s) (checkSpelling s wasAlias)
        | (s, wasAlias) <- wylieSpellings
        ]

checkSpelling :: Text -> Bool -> Assertion
checkSpelling spelling wasAlias = do
    let u = renderU (parseW spelling)
        s1 = renderW (parseU u)
        u1 = renderU (parseW s1)
        s2 = renderW (parseU u1)
    assertBool
        (unwords ["unicode leg unstable:", show spelling, show u, show s1, show u1])
        (u == u1)
    assertBool
        (unwords ["wylie leg unstable:", show spelling, show u, show s1, show s2])
        (s1 == s2)
    when (not wasAlias && T.unpack spelling `notElem` composedWylie) $
        assertBool
            ("canonical representative changed: " <> show spelling <> " -> " <> show s1)
            (s1 == spelling)

-- | Filled in stage 3: Wylie render output must stay pure ASCII.
asciiPurity :: TestTree
asciiPurity = testGroup "wylie output is pure ASCII" []

-- | Filled in stage 3: every \\uXXXX spelling round-trips to its character.
escapeSpellings :: TestTree
escapeSpellings = testGroup "\\uXXXX spellings" []

-- | Filled in stage 4: full-sentence corpus round-trips in both directions.
corpus :: TestTree
corpus = testGroup "sentence corpus" []

-- | Filled in stage 4: documented behaviour for subjoined letters.
subjoined :: TestTree
subjoined = testGroup "subjoined letters" []