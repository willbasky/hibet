-- | The reference corpora as the invariants of REFACTOR-AST §9.1 read them:
-- the Wylie and Unicode columns of the parity vectors, and the Kangyur text.
-- The cases are the reference's, the property is ours; provenance is in
-- @test/README.md@.
module Test.Corpus
    ( corpora
    , readRows
    , Row (..)
    , kangyurText
    ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | The five vector files, in the order the parity report lists them.
corpora :: [(Text, FilePath)]
corpora =
    [ ("jsewts", "test/vectors/jsewts_parity.tsv")
    , ("lingua", "test/vectors/lingua_parity.tsv")
    , ("ewts-js", "test/vectors/ewts_js_parity.tsv")
    , ("ewts-rs", "test/vectors/ewts_rs_rules.tsv")
    , ("java", "test/vectors/java_ewts.tsv")
    ]

-- | One data row of a vector file: its number and the two spellings.
data Row = Row
    { rowLine :: !Int
    , rowWylie :: !Text
    , rowUnicode :: !Text
    }

-- | The data rows of a vector file. A row the corpus leaves out of a column
-- keeps the whole line there, the way the parity reader falls back.
readRows :: FilePath -> IO [Row]
readRows path = do
    text <- readUtf8File path
    pure
        [ Row n wylie unicode
        | (n, line) <- zip [1 :: Int ..] (T.lines text)
        , not ("#" `T.isPrefixOf` line)
        , not (T.null line)
        , let wylie = T.takeWhile (/= '\t') line
              unicode = case T.splitOn "\t" line of
                (_ : u : _) -> u
                _ -> line
        ]

-- | The Kangyur text. The file opens with a byte-order mark on a line of its
-- own, so it is dropped before anything reads the text.
kangyurText :: IO Text
kangyurText = T.dropWhile (== '\xfeff') <$> readUtf8File "test/vectors/kang.txt"

-- | Read a vector file as UTF-8 regardless of the locale, so the corpora
-- decode identically everywhere.
readUtf8File :: FilePath -> IO Text
readUtf8File path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " <> show err)
        Right text -> pure text
