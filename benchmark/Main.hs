module Main where

import Conduit ((.|))
import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import Data.Conduit.Combinators (linesUnbounded)
import Data.Text (Text)
import Path (fromAbsFile)
import Path.Internal (Path (..))
import Path.IO (listDir)
import Paths_tibet (getDataFileName)
import Weigh
import Streamly
import Streamly.Prelude ((|:))

import qualified Streamly.Prelude as S
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Conduit as C
import qualified Data.Text.Encoding as T

import Handlers (Dictionary, makeTextMap, Source, Target)


main = do
    -- undefined
    -- memory
    -- memory2
    -- crit
    crit2

makeTextMapS :: Text -> Maybe Dictionary
makeTextMapS txt
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
    <$> (S.toList
    $ S.map ((\(y,x) -> (y :: Source, T.drop 1 x)) . T.span (<'|'))
    $ S.fromList
    $ T.lines txt)

makeTextMapC :: Text -> Dictionary
makeTextMapC txt = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
  (C.runConduitPure
  $ C.yield txt
  .| linesUnbounded
  .| C.mapC ((\(y,x) -> (y, T.drop 1 x)) . T.span (<'|'))
  .| C.sinkList)

toDictionaryC2 :: FilePath -> IO Dictionary
toDictionaryC2 path = C.runConduitRes
    $ C.sourceFileBS path
    .| C.decodeUtf8C
    .| C.foldMapC makeTextMapC

toDictionary :: FilePath -> IO Dictionary
toDictionary path = makeTextMap . T.decodeUtf8 <$> BS.readFile path

toDictionaryC :: FilePath -> IO Dictionary
toDictionaryC path = makeTextMapC . T.decodeUtf8 <$> BS.readFile path

toDictionaryS :: FilePath -> IO (Maybe Dictionary)
toDictionaryS path = makeTextMapS . T.decodeUtf8 <$> BS.readFile path

memory = do
    dir <- getDataFileName "dicts/"
    files <- map fromAbsFile . snd <$> listDir (Path dir)
    let toDictMeta = traverse toDictionary
    let toDictMetaC = mapConcurrently toDictionary
    let toDictMetaConduitC = mapConcurrently toDictionaryC
    let toDictMetaConduitC2 = mapConcurrently toDictionaryC2
    mainWith $ do
        io "usual + traverse" toDictMeta files
        io "usual + concurrent" toDictMetaC files
        io "conduit concurrent + usual readFile" toDictMetaConduitC files
        io "conduit concurrent 2" toDictMetaConduitC2 files

crit = do
    dir <- getDataFileName "dicts/"
    files <- map fromAbsFile . snd <$> listDir (Path dir)
    let toDictMeta = traverse toDictionary
    let toDictMetaC = mapConcurrently toDictionary
    let toDictMetaConduitC = mapConcurrently toDictionaryC
    let toDictMetaConduitC2 = mapConcurrently toDictionaryC2
    defaultMain
        [ bench "usual + traverse" $ nfIO $ toDictMeta files
        , bench "usual + concurrent" $ nfIO $ toDictMetaC files
        , bench "conduit concurrent + usual readFile" $ nfIO $ toDictMetaConduitC files
        , bench "conduit concurrent 2" $ nfIO $ toDictMetaConduitC2 files
        ]

crit2 = do
    let path = "/media/metaxis/stock/Coding/Haskell/tibet/dicts/RangjungYeshe.txt"
    txt <- T.decodeUtf8 <$> BS.readFile path
    defaultMain
        [ bench "usual" $ nf makeTextMap txt
        , bench "conduit" $ nf makeTextMapC txt
        , bench "streamly" $ nf makeTextMapS txt
        ]

memory2 = do
    let file = "/media/metaxis/stock/Coding/Haskell/tibet/dicts/RangjungYeshe.txt"
    txt <- T.decodeUtf8 <$> BS.readFile file
    mainWith $ do
        func "usual" makeTextMap txt
        func "conduit" makeTextMapC txt
        func "streamly" makeTextMapS txt
