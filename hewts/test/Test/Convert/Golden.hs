-- Ожидания: hewts — снимки конверсии поверх эталонных входов (kang.txt для
-- U→W, векторы паритета для W→U).

module Test.Convert.Golden (tests) where

import Convert (OutputFormat (..), SpellItem (..), pSentence, renderItems)
import Convert.Grammar.Parser (parseEither)
import Convert.Grammar.Syllable
    ( Role (..)
    , Stack (..)
    , Subfixes (..)
    , Syllable (..)
    , VowelSlot (..)
    , finalsTokens
    )
import Convert.Diagnostic (Diagnostics)
import Convert.Token (Token, TokenCanonical (..), tokenCanonical)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie, wylieOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
    testGroup
        "conversion golden"
        [kangyurBackToWylie, vectorsBackToUnicode, stacksSnapshot]

-- | What we currently turn the Kangyur text into in Wylie, line by line.
--
-- Nobody reads this as an expectation of correctness - the reference gives us no
-- U->W expectation of its own for this text. It exists so that the grammar
-- rework (wave 3: marked words, rendering by position) shows up as a diff of
-- this file, instead of surfacing much later as a round-trip regression.
kangyurBackToWylie :: TestTree
kangyurBackToWylie =
    goldenVsString
        "kangyur U->W snapshot"
        "test/golden/kangyur_back_to_wylie.golden"
        renderKangyur

renderKangyur :: IO BL.ByteString
renderKangyur = do
    raw <- readVector "test/vectors/kang.txt"
    let entries = zipWith entry [1 :: Int ..] (kangyurLines raw)
    pure (BL.fromStrict (TE.encodeUtf8 (T.unlines entries)))

entry :: Int -> Text -> Text
entry n raw = T.pack (show n) <> "\t" <> converted raw

converted :: Text -> Text
converted input =
    case parseEither (pSentence) (fst (tokenizeUnicode input)) of
        Left err -> "<parse error: " <> err <> ">"
        Right items -> renderItems OutWylie items

-- | What we currently turn every reference Wylie input into. The inputs are the
-- corpora Test.Parity reads, the output is our W→U conversion; the snapshot
-- exists so the AST refactor shows up as a diff here instead of as a parity
-- regression. One row: corpus, corpus line, Wylie input, our Unicode.
vectorsBackToUnicode :: TestTree
vectorsBackToUnicode =
    goldenVsString
        "reference vectors W->U snapshot"
        "test/golden/vectors_back_to_unicode.golden"
        renderVectors

renderVectors :: IO BL.ByteString
renderVectors = do
    rows <- concat <$> mapM renderCorpus vectors
    pure (BL.fromStrict (TE.encodeUtf8 (T.unlines rows)))

-- | The corpora and their names, in the order the report lists them.
vectors :: [(Text, FilePath)]
vectors =
    [ ("jsewts", "test/vectors/jsewts_parity.tsv")
    , ("lingua", "test/vectors/lingua_parity.tsv")
    , ("ewts-js", "test/vectors/ewts_js_parity.tsv")
    , ("ewts-rs", "test/vectors/ewts_rs_rules.tsv")
    , ("java", "test/vectors/java_ewts.tsv")
    ]

renderCorpus :: (Text, FilePath) -> IO [Text]
renderCorpus (name, path) = do
    text <- readVector path
    pure
        [ name <> "\t" <> T.pack (show n) <> "\t" <> wylie <> "\t" <> convertedW2U wylie
        | (n, line) <- zip [1 :: Int ..] (T.lines text)
        , not ("#" `T.isPrefixOf` line)
        , not (T.null line)
        , wylie : _ <- [T.splitOn "\t" line]
        ]

convertedW2U :: Text -> Text
convertedW2U input =
    case parseEither (pSentence) (fst (tokenizeWylie input)) of
        Left err -> "<parse error: " <> err <> ">"
        Right items -> renderItems OutUnicode items

-- | The Kangyur corpus, the same way Test.Parity reads it: the file starts with
-- a byte-order mark on a line of its own, so it is dropped before splitting.
kangyurLines :: Text -> [Text]
kangyurLines = filter (not . T.null) . T.lines . dropBom

dropBom :: Text -> Text
dropBom = T.dropWhile (== '\xfeff')

-- | Read a vector file as UTF-8 regardless of the locale, so the corpora decode
-- identically everywhere. Same helper as in Test.Parity; kept local so that the
-- two test modules do not depend on each other.
readVector :: FilePath -> IO Text
readVector path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " ++ show err)
        Right text -> pure text

-- | The stacks of every corpus syllable: the oracle REFACTOR-AST step 4 must
-- reproduce when the grammar builds them itself. One row: corpus, corpus line,
-- syllable index in the line, the stacks. A stack reads
-- @Role:letters^vowel#finals@ - letters in writing order (superfix, root,
-- subfixes), @^@ for the caret, @~@ for the implicit @a@ (a written vowel is
-- @:@ and its mnemonics), @#@ and the final mnemonics in slot order.
stacksSnapshot :: TestTree
stacksSnapshot =
    goldenVsString
        "stack snapshot (step 4 oracle)"
        "test/golden/stacks.golden"
        renderStacks

renderStacks :: IO BL.ByteString
renderStacks = do
    wylieRows <- concat <$> mapM (renderStackCorpus tokenizeWylie wylieColumn) vectors
    unicodeRows <- concat <$> mapM (renderStackCorpus tokenizeUnicode unicodeColumn) vectors
    kangyur <- renderStackCorpus tokenizeUnicode id ("kangyur", "test/vectors/kang.txt")
    pure
        (BL.fromStrict (TE.encodeUtf8 (T.unlines (wylieRows <> unicodeRows <> kangyur))))

renderStackCorpus :: (Text -> ([Token], Diagnostics)) -> (Text -> Text) -> (Text, FilePath) -> IO [Text]
renderStackCorpus tokenize column (name, path) = do
    text <- readVector path
    pure
        [ name
            <> "\t"
            <> T.pack (show n)
            <> "\t"
            <> T.pack (show i)
            <> "\t"
            <> syllableStacks s
        | (n, line) <- zip [1 :: Int ..] (T.lines text)
        , not ("#" `T.isPrefixOf` line)
        , not (T.null line)
        , let input = column line
        , not (T.null input)
        , Right items <- [parseEither pSentence (fst (tokenize input))]
        , (i, s) <- zip [1 :: Int ..] [s | SyllableItem s <- items]
        ]

wylieColumn :: Text -> Text
wylieColumn = T.takeWhile (/= '\t')

unicodeColumn :: Text -> Text
unicodeColumn line = case T.splitOn "\t" line of
    (_ : u : _) -> u
    _ -> line

syllableStacks :: Syllable -> Text
syllableStacks = T.unwords . map stackText . toList . sylStacks

stackText :: Stack -> Text
stackText st =
    roleName (stRole st)
        <> ":"
        <> letters
        <> caret
        <> vowel
        <> finals
    where
        letters =
            T.intercalate
                "+"
                [ mnemonic tok
                | tok <-
                    maybe [] pure (stSuperfix st)
                        <> maybe [] pure (stRoot st)
                        <> subfixTokens (stSubfixes st)
                ]
        caret = maybe "" (const "^") (stCaret st)
        vowel = case stVowel st of
            Nothing -> ""
            Just VowelImplicit -> "~"
            Just (VowelWritten tok rest) -> ":" <> T.intercalate "+" (map mnemonic (tok : rest))
        finals = case finalsTokens (stFinals st) of
            [] -> ""
            ts -> "#" <> T.concat (map mnemonic ts)

subfixTokens :: Subfixes -> [Token]
subfixTokens NoSubfix = []
subfixTokens (OneSubfix a) = [a]
subfixTokens (TwoSubfix a b) = [a, b]
subfixTokens (ThreeSubfix a b c) = [a, b, c]

roleName :: Role -> Text
roleName RolePrefix = "Prefix"
roleName RoleRoot = "Root"
roleName RoleSuffix = "Suffix"
roleName RolePostfix = "Postfix"

mnemonic :: Token -> Text
mnemonic tok = fromMaybe "" (wylieOf (tokenCanonical tok))
