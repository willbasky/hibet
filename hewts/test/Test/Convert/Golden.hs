module Test.Convert.Golden (tests) where

import Convert (OutputFormat (..), SpellItem (..), pSentence, renderItems)
import Convert.Grammar.Parser (parseEither)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests = testGroup "conversion golden" [kangyurBackToWylie]

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
    case parseEither pSentence (fst (tokenizeUnicode input)) of
        Left err -> "<parse error: " <> err <> ">"
        Right items -> renderItems OutWylie items

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
