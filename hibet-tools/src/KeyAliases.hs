-- | Regenerate @hibet-data/key-aliases.tsv@ from the dictionaries.
--
-- Dictionary keys keep the spelling of their source, but a Unicode query goes
-- through hewts and gets canonical Wylie. For most keys the two spellings are
-- the same; where they differ, this table records the legacy spelling and the
-- key a Unicode query produces, so the loader can add the canonical key next
-- to the legacy one. Only faithful pairs are written: both spellings must
-- hold the same syllables. Pairs that would change the word (hewts' known
-- round-trip gaps) are reported and skipped.
module KeyAliases
    ( generateAliases
    ) where

import Convert
    ( Settings (..)
    , convertWylieToUnicode
    , defaultSettings
    , syllables
    )
import Io (readUtf8, writeUtf8)
import Translator (tibetToWylieKey)

import Control.Exception (evaluate, throwIO)
import Control.Monad (foldM, forM_)
import qualified Data.HashMap.Strict as HMS
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTime)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

generateAliases :: FilePath -> FilePath -> IO ()
generateAliases dictDir outFile = do
    files <- sortOn id . filter (\f -> take 1 f /= ".") <$> listDirectory dictDir
    keys <- foldM addFileKeys HMS.empty files
    TIO.putStr
        (T.pack (printf "files: %d, distinct keys: %d\n" (length files) (HMS.size keys)))

    t0 <- getMonotonicTime
    let parsed =
            [ (key, tibet)
            | key <- HMS.keys keys
            , Just tibet <- [tibetOf key]
            ]
        unreadable = HMS.size keys - length parsed
        rows =
            [ (key, canonical)
            | (key, tibet) <- parsed
            , Right canonical <- [tibetToWylieKey tibet]
            , not (T.null canonical)
            , canonical /= key
            ]
        (faithful, lossy) = foldr split ([], []) rows
        split row@(key, canonical) (good, bad)
            | sameWord key canonical = (row : good, bad)
            | otherwise = (good, row : bad)
    nFaithful <- evaluate (length faithful)
    nLossy <- evaluate (length lossy)
    t1 <- getMonotonicTime

    let output =
            T.unlines
                [ key <> "\t" <> canonical
                | (key, canonical) <- sortOn fst faithful
                ]
    writeUtf8 outFile output

    TIO.putStr (T.pack (printf "scan: %.1f s\n" (t1 - t0 :: Double)))
    TIO.putStr (T.pack (printf "keys hewts cannot read: %d\n" unreadable))
    TIO.putStr
        ( T.pack
            ( printf
                "same spelling (no alias needed): %d\n"
                (HMS.size keys - length rows - unreadable)
            )
        )
    TIO.putStr (T.pack (printf "aliases written: %d\n" nFaithful))
    TIO.putStr (T.pack (printf "skipped, canonical changes the word: %d\n" nLossy))
    forM_ (take 10 lossy) $ \(key, canonical) ->
        TIO.putStrLn ("  " <> T.pack (show key) <> " -> " <> T.pack (show canonical))
    where
        addFileKeys acc file = do
            text <- readUtf8 (dictDir </> file) >>= either throwIO pure
            let new =
                    HMS.fromList
                        [ (key, ())
                        | line <- T.lines text
                        , let key = keyOf line
                        , not (T.null key)
                        ]
                merged = HMS.union acc new
            HMS.size merged `seq` pure merged

-- | The Tibetan for a Wylie key, or 'Nothing' when hewts cannot read the key.
tibetOf :: T.Text -> Maybe T.Text
tibetOf key = case convertWylieToUnicode defaultSettings{withChecks = False} key of
    Left _ -> Nothing
    Right (tibet, _) -> Just tibet

keyOf :: T.Text -> T.Text
keyOf = T.dropWhileEnd (== '\r') . fst . T.span (/= '|')

-- | Same word: hewts reads the same syllables from both spellings. Punctuation
-- and spacing do not count, the query path drops them too.
sameWord :: T.Text -> T.Text -> Bool
sameWord key canonical =
    case (tibetOf key, tibetOf canonical) of
        (Just fromKey, Just fromCanonical) ->
            syllables fromKey == syllables fromCanonical
        _ -> False
