-- Ожидания: hewts — инварианты разбора и печати (токены не теряются, W→W канон, конверсия — неподвижная точка); входы — генератор форм.

-- | Волна 10: структурный fuzz. Волна 7 гоняла по кругу эталонный текст, эта
-- гоняет формы, которых в тексте может и не быть: случайные слоги в порядке
-- книги (согласные, гласная, финал), склеенные в документ. Вход генерирует
-- falsify (внутренний
-- integrated shrinking: падение сокращается до минимальной формы, seed
-- печатается и перезапускается через @--falsify-replay@).
--
-- Проверяем четыре инварианта, каждый — факт кода, а не эталон:
--
-- 1. разбор тотален и не теряет букв: каждый токен входа попадает в какую-то
--    статью разбора;
-- 2. W→W после первого прохода — неподвижная точка (канон стабилен);
-- 3. конверсия W→U→W→U после первого прохода ничего не меняет;
-- 4. конверсия U→W→U→W после первого прохода ничего не меняет.
--
-- Домен — буквы и финалы. Две вещи из Wylie-домена исключены намеренно:
-- escape-запись (@\u0Fxx@) и голая буква @a@ — та самая, что пишет неявную
-- гласную и в Unicode становится а-ченом; через петлю она не устойчива (см.
-- Волну 5 и «Волна 5.1» в @docs/archive/hewts/ROADMAP-waves.md@), и её
-- поведение закреплено в
-- @Test.Tokenizer.RoundTrip@ отдельно. Unicode-домен — те же формы, но
-- глифами: стек, гласная, финал; первая буква стека всегда полная, потому что
-- подписной без буквы — отдельный разобранный случай (волна 5.4). Стеки
-- берутся только те, что Wylie-нотация пишет слитно ('solidStacks'): запись
-- через @+@ разбор читает как отдельное слово, и общей неподвижной точки там
-- нет (открытый вопрос волны).
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
import Convert.Render.Wylie.Notation (solidStacks)
import Convert.Token
    ( Consonant
    , FinalMark
    , Token
    , TokenCanonical (..)
    , Vowel
    , toSubjoined
    , tokenRaw
    )
import Convert.Token.Unicode (unicodeOf)
import Convert.Token.Wylie (wylieConsonant, wylieFinal, wylieVowel)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Falsify (Property, assert, gen)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Grammar.Book (bookSyllable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

tests :: TestTree
tests =
    testGroup
        "fuzz (structures)"
        [ testProperty "every token of the input is kept" propTokens
        , testProperty "W->W is canonical after one pass" propWylieCanonical
        , testProperty "W->U->W->U is a fixed point after one pass" propConversion
        , testProperty "U->W->U->W is a fixed point after one pass" propUnicodeConversion
        , testProperty
            "book documents keep the canon and the W->U->W->U loop"
            propBookDocument
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
consonants = NE.fromList (map wylieConsonant wylieDomainConsonants)
vowels = NE.fromList (map wylieVowel wylieDomainVowels)
finals = NE.fromList (map wylieFinal wylieDomainFinals)

-- | The canonical alphabet both properties share. The bare @a@ and the
-- escape-spelled bundled letters stay out, so what the Wylie side writes is
-- always a plain spelling and what the Unicode side writes is always a glyph
-- Wylie names with a letter.
wylieDomainConsonants :: [Consonant]
wylieDomainConsonants = [c | c <- [minBound .. maxBound], plain (wylieConsonant c)]

wylieDomainVowels :: [Vowel]
wylieDomainVowels = [v | v <- [minBound .. maxBound], plain (wylieVowel v)]

wylieDomainFinals :: [FinalMark]
wylieDomainFinals = [f | f <- [minBound .. maxBound], plain (wylieFinal f)]

plain :: Text -> Bool
plain s =
    s /= "a"
        && not (T.any (`elem` ("\\?^~`&|0=#L:" :: String)) s)

-- | The same document shape written in glyphs.
unicodeDocument :: Gen Text
unicodeDocument = T.intercalate " " <$> Gen.list (Range.inclusive (1, 6)) unicodeSyllable

-- | A syllable as the script writes it: a stack, an optional vowel sign and
-- an optional final. The stack's first letter is always full: a subjoined
-- letter with no letter to attach to is the escape case of wave 5.4.
unicodeSyllable :: Gen Text
unicodeSyllable = do
    stack <- Gen.elem unicodeStacks
    vowel <- T.concat <$> Gen.list (Range.inclusive (0, 1)) (Gen.elem unicodeVowels)
    final <- T.concat <$> Gen.list (Range.inclusive (0, 1)) (Gen.elem unicodeFinals)
    pure (stack <> vowel <> final)

-- | The stacks the Unicode side writes: a full letter and up to two subjoined
-- ones, restricted to the combinations the Wylie notation writes solid
-- ('solidStacks', built from the book's tables). A combination outside them is
-- spelled with a @+@ that the parser reads as a separate word, so the two
-- scripts have no fixed point to test there until the roles are reconciled
-- (the open question of the wave).
unicodeStacks :: NonEmpty Text
unicodeStacks = NE.fromList (map glyphRun (filter solidRun letterRuns))
    where
        letters = wylieDomainConsonants
        letterRuns =
            [[c] | c <- letters]
                <> [[c, s] | c <- letters, s <- letters]
                <> [[c, s, t] | c <- letters, s <- letters, t <- letters]

        solidRun [_] = True
        solidRun run =
            Set.member (T.intercalate "+" (map wylieConsonant run)) solidStacks

        glyphRun (c : cs) =
            maybe "" id (unicodeOf (TcConsonant c))
                <> T.concat
                    [ maybe "" id (unicodeOf (TcSubConsonant s))
                    | c' <- cs
                    , Just s <- [toSubjoined c']
                    ]
        glyphRun [] = ""

-- | The rest of the alphabet in glyphs, taken from the table printing uses so
-- that a glyph cannot drift from its canonical.
unicodeVowels, unicodeFinals :: NonEmpty Text
unicodeVowels = glyphs (map TcVowel wylieDomainVowels)
unicodeFinals = glyphs (map TcFinal wylieDomainFinals)

glyphs :: [TokenCanonical] -> NonEmpty Text
glyphs canonicals = NE.fromList [g | c <- canonicals, Just g <- [unicodeOf c]]

propTokens :: Property ()
propTokens = documentProperty tokenCheck

propWylieCanonical :: Property ()
propWylieCanonical = documentProperty wylieCanonicalCheck

propConversion :: Property ()
propConversion = documentProperty conversionCheck

propUnicodeConversion :: Property ()
propUnicodeConversion = documentPropertyWith unicodeDocument unicodeConversionCheck

-- | The same invariants over documents built by the book's role generator, so
-- all 37 structures reach the random run (the letter-run generator above only
-- hits 1, 8, 17, 28 and 38).
-- | The token check stays out: the book generator spells a bare root's
-- implicit a as Wylie @a@, which the grammar reads as 'VowelImplicit' and not
-- as a token; that fact is covered by 'Test.Tokenizer.RoundTrip'.
propBookDocument :: Property ()
propBookDocument = documentPropertyWith bookDocument $ \input ->
    wylieCanonicalCheck input <> conversionCheck input

bookDocument :: Gen Text
bookDocument = T.intercalate " " <$> Gen.list (Range.inclusive (1, 6)) bookSyllable

tokenCheck :: Text -> [String]
tokenCheck input =
    case parseW input of
        Left err -> ["the parse failed: " <> T.unpack err]
        Right items ->
            let dropped = droppedTokens (fst (tokenizeWylie input)) (concatMap itemTokens items)
             in ["dropped " <> show (map tokenRaw dropped) | not (null dropped)]

wylieCanonicalCheck :: Text -> [String]
wylieCanonicalCheck input =
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

conversionCheck :: Text -> [String]
conversionCheck input =
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

unicodeConversionCheck :: Text -> [String]
unicodeConversionCheck input =
    case passU input of
        Left err -> ["the conversion failed: " <> T.unpack err]
        Right (w1, w2)
            | w1 == w2 -> []
            | otherwise ->
                ["the Wylie script is not stable: " <> show w1 <> " -> " <> show w2]

-- | The U→W→U→W leg: the format opposite the input is the one held steady.
passU :: Text -> Either Text (Text, Text)
passU input = do
    w1 <- stepU input
    u1 <- stepW w1
    w2 <- stepU u1
    pure (w1, w2)

stepW :: Text -> Either Text Text
stepW = fmap fst . convertWylieToUnicode defaultSettings

stepU :: Text -> Either Text Text
stepU = fmap fst . convertUnicodeToWylie defaultSettings

-- | A property over generated documents: the predicate names the input, so the
-- shrunk counterexample is printed with the document it describes.
documentProperty :: (Text -> [String]) -> Property ()
documentProperty = documentPropertyWith document

documentPropertyWith :: Gen Text -> (Text -> [String]) -> Property ()
documentPropertyWith documentGen check = do
    input <- gen documentGen
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
