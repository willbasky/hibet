module Test.Tokenizer.RoundTrip (tests) where

import Control.Monad (when)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

import Convert
    ( OutputFormat (..)
    , SpellItem (..)
    , pSentence
    , renderItems
    , splitSentences
    )
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Token
import Convert.Tokenizer.Unicode (unicodeOf)
import Convert.Tokenizer.Wylie
    ( tokenizeWylie
    , wylieConsonant
    , wylieConsonantAliases
    , wylieFinal
    , wylieHalfNumber
    , wylieNumber
    , wylieOrnament
    , wyliePunctuation
    , wylieSanskritMark
    , wylieSign
    , wylieSpace
    , wylieSymbol
    , wylieVowel
    )

tests :: TestTree
tests =
    testGroup
        "round-trip"
        [ unicodeToWylieToUnicode
        , wylieToUnicodeToWylie
        , compounds
        ]

-- | Parse Tibetan text as Unicode and run the token-level grammar.
parseU :: Text -> [SpellItem]
parseU input =
    case splitSentences input of
        Left err -> error ("RoundTrip.parseU: " <> show err)
        Right items -> items

-- | Parse Wylie text and run the token-level grammar.
--
-- Wylie text is read with the Wylie spelling, which is what lets the letter
-- @a@ stand for "no vowel here" and be kept in the word as an
-- 'ImplicitVowel' - the spelling is fully covered, and the renderer skips it.
parseW :: Text -> [SpellItem]
parseW input =
    case parseEither (pSentence Wylie) (fst (tokenizeWylie input)) of
        Left err -> error ("RoundTrip.parseW: " <> show err)
        Right items -> items

renderW :: [SpellItem] -> Text
renderW = renderItems OutWylie

renderU :: [SpellItem] -> Text
renderU = renderItems OutUnicode

-- | Every canonical with both a Unicode spelling and a Wylie spelling.
-- @SubConsonant@ is handled in the later waves: subjoined letters have no
-- Wylie spelling of their own until the composition waves, so they are not
-- covered here.
canonicals :: [TokenCanonical]
canonicals =
    [TcConsonant c | c <- [minBound .. maxBound :: Consonant]]
        <> [TcVowel v | v <- [minBound .. maxBound :: Vowel]]
        <> [TcFinal f | f <- [minBound .. maxBound :: FinalMark]]
        <> [TcNumber n | n <- [minBound .. maxBound :: Number]]
        <> [TcHalfNumber h | h <- [minBound .. maxBound :: HalfNumber]]
        <> [TcPunctuation p | p <- [minBound .. maxBound :: PunctuationMark]]
        <> [TcSign s | s <- [minBound .. maxBound :: SignMark]]
        <> [TcSanskritMark m | m <- [minBound .. maxBound :: SanskritMark]]
        <> [TcOrnament o | o <- [minBound .. maxBound :: OrnamentMark]]
        <> [TcSpace m | m <- [minBound .. maxBound :: SpaceMark]]
        <> [TcSymbol s | s <- [minBound .. maxBound :: SymbolMark]]

unicodeToWylieToUnicode :: TestTree
unicodeToWylieToUnicode =
    testGroup
        "unicode -> wylie -> unicode is identity"
        [ testCase (show c) $
            let u =
                    case unicodeOf c of
                        Just x -> x
                        Nothing -> error ("no unicode spelling for " <> show c)
             in checkUnicodeIdentity c u (show c)
        | c <- canonicals
        ]

checkUnicodeIdentity :: TokenCanonical -> Text -> String -> Assertion
checkUnicodeIdentity canon input label =
    let w = renderW (parseU input)
        back = renderU (parseW w)
        -- A lone vowel renders with the a-chen in front (wylie "i" -> ཨི),
        -- exactly what the references write, so only the vowel glyphs gain it.
        want =
            case canon of
                TcVowel _ -> "ཨ" <> input
                _ -> input
        msg =
            unwords
                [ "unicode identity failed:"
                , label
                , "in=" <> T.unpack input
                , "w=" <> T.unpack w
                , "back=" <> T.unpack back
                ]
     in assertBool msg (back == want)

-- | Every Wylie spelling the tokenizer accepts, with @True@ when the spelling
-- is an alias that normalizes to a canonical representative on the first pass.
wylieSpellings :: [(Text, Bool)]
wylieSpellings = HS.toList . HS.fromList $ reps' <> aliasKeys
    where
        reps' = [(s, False) | s <- wylieReps]
        aliasKeys = [(s, True) | s <- wylieAliasKeys]

wylieReps :: [Text]
wylieReps =
    [wylieConsonant c | c <- [minBound .. maxBound :: Consonant]]
        <> [wylieVowel v | v <- [minBound .. maxBound :: Vowel]]
        <> [wylieFinal f | f <- [minBound .. maxBound :: FinalMark]]
        <> [wylieNumber n | n <- [minBound .. maxBound :: Number]]
        <> [wylieHalfNumber h | h <- [minBound .. maxBound :: HalfNumber]]
        <> [wyliePunctuation p | p <- [minBound .. maxBound :: PunctuationMark]]
        <> [wylieSign s | s <- [minBound .. maxBound :: SignMark]]
        <> [wylieSanskritMark m | m <- [minBound .. maxBound :: SanskritMark]]
        <> [wylieOrnament o | o <- [minBound .. maxBound :: OrnamentMark]]
        <> [wylieSymbol s | s <- [minBound .. maxBound :: SymbolMark]]
        <> [wylieSpace m | m <- [minBound .. maxBound :: SpaceMark]]

wylieAliasKeys :: [Text]
wylieAliasKeys = map fst wylieConsonantAliases

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
    when (not wasAlias) $
        assertBool
            ("canonical representative changed: " <> show spelling <> " -> " <> show s1)
            (s1 == canonicalRepresentative spelling)

-- | The stable Wylie spelling of a lone mark (the spelling the renderer
-- keeps returning once a chain enters it). A lone a-chen stays "a"; the
-- ordinary vowels keep their own spelling (the a-chen that carries them
-- in Unicode is written back out, not forward: "A" -> ཨཱ -> "A"); a mark
-- a bare a-chen does not absorb keeps the a-chen in front ("ai" -> ཨཻ ->
-- "aai"); and a lone consonant keeps its own spelling - the renderer
-- never appends an implicit vowel to a closing letter (ས -> "s"),
-- matching what the Wylie arms read back.
canonicalRepresentative :: Text -> Text
canonicalRepresentative s
    | s `elem` ["ai", "au", "-i"] = "a" <> s
    | otherwise = s

-- | Compound (expanded) spellings: R"gh" -> R"གྷ" -> [Cg, SCh] -> R"གྷ".
-- The Wylie leg is not stable yet (subjoined letters have no Wylie spelling
-- until the later waves), so we pin the Unicode leg only.
compounds :: TestTree
compounds =
    testGroup
        "compound spellings"
        [ testCase (T.unpack s) (checkCompoundSpelling s)
        | s <- compoundSpellings
        ]

compoundSpellings :: [Text]
compoundSpellings =
    [ "gh"
    , "g+h"
    , "Dh"
    , "D+h"
    , "dh"
    , "d+h"
    , "bh"
    , "b+h"
    , "dzh"
    , "dz+h"
    , "k+Sh"
    , "-dh"
    , "-d+h"
    , "I"
    , "U"
    , "E"
    , "O"
    , "-I"
    , "r-i"
    , "r-I"
    , "l-i"
    , "l-I"
    , "f"
    , "v"
    ]

checkCompoundSpelling :: Text -> Assertion
checkCompoundSpelling spelling = do
    let u = renderU (parseW spelling)
        back = renderU (parseU u)
    assertBool
        (unwords ["compound unicode leg unstable:", show spelling, show u, show back])
        (u == back)
