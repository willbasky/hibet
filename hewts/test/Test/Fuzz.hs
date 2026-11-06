-- Ожидания: hewts — инвариант «второй проход ничего не меняет»; входы — генератор с зерном, текст — Кангьюр.

-- | Волна 7: нечёткий прогон (fuzz). Случайные куски эталонного текста гоняем
-- туда-обратно и требуем, чтобы второй проход ничего не менял: конверсия
-- должна быть неподвижной точкой после первой петли.
--
-- Домен — только эталонный текст: вырожденные формы (escape-запись знаков без
-- Wylie-написания, чужой текст в скобках, пара а-чунгов) через петлю не
-- устойчивы — это разобранные формы Волны 5
-- (@docs/archive/hewts/ROADMAP-waves.md@) и §6.2
-- @docs/archive/hewts/REFACTOR-AST.md@, а не дефект этой проверки. Их сюда не
-- подмешиваем.
module Test.Fuzz (tests) where

import Convert (convertUnicodeToWylie, convertWylieToUnicode, defaultSettings)
import Data.Bits (shiftR)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Test.Corpus (kangyurText, kangyurWylie)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "fuzz"
        [ testCase "unicode slices: the second pass changes nothing" caseUnicode
        , testCase "wylie slices: the second pass changes nothing" caseWylie
        ]

-- | Real text, cut at random: the slices are the language as it is written, the
-- seed is pinned so a failure reproduces.
--
-- shortcut: one pinned seed per direction, so the set of slices is fixed;
-- widen it (another seed, more slices) if it stops finding new shapes.
seedUnicode, seedWylie :: Word64
seedUnicode = 42
seedWylie = 7

-- | How many slices each direction draws.
slices :: Int
slices = 500

caseUnicode :: Assertion
caseUnicode = do
    text <- kangyurText
    checkAll "unicode" unstableUnicode seedUnicode (syllablesOn "་" text)

caseWylie :: Assertion
caseWylie = do
    text <- kangyurWylie
    checkAll "wylie" unstableWylie seedWylie (syllablesOn " " text)

-- | The pieces a slice is built from: the corpus text cut at its separator.
syllablesOn :: Text -> Text -> [Text]
syllablesOn sep =
    filter (not . T.null) . concatMap (T.splitOn sep) . T.lines

-- | Random slices, one per sample, each joined into a document.
drawSlices :: Word64 -> [Text] -> [Text]
drawSlices seed pieces = go (Rng seed) slices
    where
        go _ 0 = []
        go r k = let (slice, r') = draw r in slice : go r' (k - 1)
        draw r0 =
            let (start, r1) = pick (length pieces) r0
                (len, r2) = pick 12 r1
             in (T.intercalate "\n" (take (len + 1) (drop start (cycle pieces))), r2)

checkAll :: String -> (Text -> Maybe String) -> Word64 -> [Text] -> Assertion
checkAll label property seed pieces =
    let bad =
            [ (input, reason)
            | input <- drawSlices seed pieces
            , Just reason <- [property input]
            ]
        shown = take 5 bad
        more =
            if length bad > length shown
                then "… (" <> show (length bad - length shown) <> " more)"
                else ""
     in assertBool
            ( label
                <> ": "
                <> show (length bad)
                <> " of "
                <> show slices
                <> " slices are not a fixed point:\n"
                <> unlines [T.unpack input <> ": " <> reason | (input, reason) <- shown]
                <> more
            )
            (null bad)

-- | Unicode in, Wylie out, Unicode again: the first pass must be the last one.
unstableUnicode :: Text -> Maybe String
unstableUnicode input = case unicodePass input of
    Left err -> Just ("left: " <> T.unpack err)
    Right (u1, u2)
        | u1 == u2 -> Nothing
        | otherwise -> Just ("changed: " <> show u1 <> " -> " <> show u2)

unicodePass :: Text -> Either Text (Text, Text)
unicodePass input = do
    w <- stepU input
    u1 <- stepW w
    u2 <- stepW =<< stepU u1
    pure (u1, u2)

-- | Wylie in, Unicode out, Wylie again: the same invariant, the other way.
unstableWylie :: Text -> Maybe String
unstableWylie input = case wyliePass input of
    Left err -> Just ("left: " <> T.unpack err)
    Right (w1, w2)
        | w1 == w2 -> Nothing
        | otherwise -> Just ("changed: " <> show w1 <> " -> " <> show w2)

wyliePass :: Text -> Either Text (Text, Text)
wyliePass input = do
    u1 <- stepW input
    w1 <- stepU u1
    w2 <- stepU =<< stepW w1
    pure (w1, w2)

stepW :: Text -> Either Text Text
stepW = fmap fst . convertWylieToUnicode defaultSettings

stepU :: Text -> Either Text Text
stepU = fmap fst . convertUnicodeToWylie defaultSettings

-- | A linear congruential generator over 'Word64': the suite needs reproducible
-- random text, not a random package. The high bits are the ones worth taking.
newtype Rng = Rng Word64

next :: Rng -> (Word64, Rng)
next (Rng s) =
    let s' = s * 6364136223846793005 + 1442695040888963407
     in (s' `shiftR` 33, Rng s')

pick :: Int -> Rng -> (Int, Rng)
pick n r0 = let (v, r1) = next r0 in (fromIntegral v `mod` n, r1)
