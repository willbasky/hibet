-- Ожидания: hewts — инварианты разбора и печати (токены не теряются, W→W канон, конверсия — неподвижная точка); входы — генератор форм.

-- | Волна 10: структурный fuzz. Волна 7 гоняла по кругу эталонный текст, эта
-- гоняет формы, которых в тексте может и не быть: случайные слоги в порядке
-- книги (согласные, гласная, финал), склеенные в документ. Вход генерирует
-- falsify (внутренний
-- integrated shrinking: падение сокращается до минимальной формы, seed
-- печатается и перезапускается через @--falsify-replay@).
--
-- Проверяем три инварианта, каждый — факт кода, а не эталон:
--
-- 1. разбор тотален и не теряет букв: каждый токен входа попадает в какую-то
--    статью разбора;
-- 2. W→W после первого прохода — неподвижная точка (канон стабилен);
-- 3. конверсия W→U→W→U после первого прохода ничего не меняет.
--
-- Домен — буквы и финалы. Две вещи из него исключены намеренно: escape-запись
-- (@\u0Fxx@) и голая буква @a@ — та самая, что пишет неявную гласную и в
-- Unicode становится а-ченом; через петлю она не устойчива (см. Волну 5 и
-- «Волна 5.1» в @ROADMAP.md@), и её поведение закреплено в
-- @Test.Tokenizer.RoundTrip@ отдельно.
module Test.Fuzz.Structure (tests) where

import Convert
    ( OutputFormat (..)
    , SpellItem (..)
    , convertUnicodeToWylie
    , convertWylieToUnicode
    , defaultSettings
    , pSentence
    , renderItems
    )
import Convert.Grammar.Parser (parseEither)
import qualified Convert.Grammar.Syllable as Ast
import Convert.Token (Token, tokenRaw)
import Convert.Token.Wylie (wylieConsonant, wylieFinal, wylieVowel)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Falsify (Property, assert, gen)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

tests :: TestTree
tests =
    testGroup
        "fuzz (structures)"
        [ testProperty "every token of the input is kept" propTokens
        , testProperty "W->W is canonical after one pass" propWylieCanonical
        , testProperty "W->U->W->U is a fixed point after one pass" propConversion
        ]

-- | A document of random syllables: the shape of real text, the content random.
document :: Gen Text
document = T.intercalate " " <$> Gen.list (Range.inclusive (1, 6)) syllable

-- | A syllable in the order the book writes one: one to three consonants (the
-- root with a prefix or a superfix before it), then an optional vowel, then an
-- optional final. The order matters: a vowel after a final is no Tibetan
-- syllable, and the grammar reads it as a word of its own, where the loop does
-- not have to be stable (falsify found @rM-i@ exactly this way).
syllable :: Gen Text
syllable = do
    consonants <- T.concat <$> Gen.list (Range.inclusive (1, 3)) consonant
    vowel <- T.concat <$> Gen.list (Range.inclusive (0, 1)) vowelLetter
    final <- T.concat <$> Gen.list (Range.inclusive (0, 1)) finalLetter
    pure (consonants <> vowel <> final)

consonant, vowelLetter, finalLetter :: Gen Text
consonant = Gen.elem consonants
vowelLetter = Gen.elem vowels
finalLetter = Gen.elem finals

-- | The alphabet: the spellings the tokenizer reads as letters.
--
-- shortcut: escape spellings and the bare letter @a@ stay out, so the input
-- is always plain letters; revisit if a property needs them.
consonants, vowels, finals :: NonEmpty Text
consonants = NE.fromList $ filter plain [wylieConsonant c | c <- [minBound .. maxBound]]
vowels = NE.fromList $ filter plain [wylieVowel v | v <- [minBound .. maxBound]]
finals = NE.fromList $ filter plain [wylieFinal f | f <- [minBound .. maxBound]]

plain :: Text -> Bool
plain s =
    s /= "a"
        && not (T.any (`elem` ("\\?^~`&|0=#L:" :: String)) s)

propTokens :: Property ()
propTokens = documentProperty $ \input ->
    case parseW input of
        Left err -> ["the parse failed: " <> T.unpack err]
        Right items ->
            let dropped = droppedTokens (fst (tokenizeWylie input)) (concatMap itemTokens items)
             in ["dropped " <> show (map tokenRaw dropped) | not (null dropped)]

propWylieCanonical :: Property ()
propWylieCanonical = documentProperty $ \input ->
    case parseW input of
        Left err -> ["the parse failed: " <> T.unpack err]
        Right items ->
            let s1 = renderW items
             in case parseW s1 of
                    Left err -> ["the canon does not read back: " <> T.unpack err]
                    Right items1 ->
                        let s2 = renderW items1
                         in [ "the canon is not stable: " <> show s1 <> " -> " <> show s2
                            | s1 /= s2
                            ]

propConversion :: Property ()
propConversion = documentProperty $ \input ->
    case pass input of
        Left err -> ["the conversion failed: " <> T.unpack err]
        Right (u1, u2)
            | u1 == u2 -> []
            | otherwise -> ["the script is not stable: " <> show u1 <> " -> " <> show u2]

-- | The W→U→W→U leg of the public conversion, internals included.
pass :: Text -> Either Text (Text, Text)
pass input = do
    u1 <- stepW input
    w1 <- stepU u1
    u2 <- stepW w1
    pure (u1, u2)

stepW :: Text -> Either Text Text
stepW = fmap fst . convertWylieToUnicode defaultSettings

stepU :: Text -> Either Text Text
stepU = fmap fst . convertUnicodeToWylie defaultSettings

-- | A property over generated documents: the predicate names the input, so the
-- shrunk counterexample is printed with the document it describes.
documentProperty :: (Text -> [String]) -> Property ()
documentProperty check = do
    input <- gen document
    assert $
        P.lam
            ( \value ->
                let reasons = check value
                 in if null reasons then P.pass else P.fail (unlines reasons)
            )
            P..$ ("input", input)

parseW :: Text -> Either Text [SpellItem]
parseW = parseEither pSentence . fst . tokenizeWylie

renderW :: [SpellItem] -> Text
renderW = renderItems OutWylie

itemTokens :: SpellItem -> [Token]
itemTokens (SyllableItem s) = toList (Ast.flatten (Ast.sylStacks s))
itemTokens (InvalidSyllableItem ts _) = ts
itemTokens (Number ts) = ts
itemTokens (Punct ts) = ts
itemTokens (Other ts) = ts

-- | The tokens of the input that no item holds, as a multiset difference.
droppedTokens :: [Token] -> [Token] -> [Token]
droppedTokens input held = foldl dropOne held input
    where
        dropOne [] tok = [tok]
        dropOne (h : hs) tok
            | h == tok = hs
            | otherwise = h : dropOne hs tok
