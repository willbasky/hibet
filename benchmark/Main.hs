module Main where

import Conduit ((.|))
import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import Data.Bifunctor (second)
import Data.Conduit.Combinators (linesUnbounded)
import Data.Text (Text)
import Path
import Path.IO
import Paths_hibet (getDataFileName)
import Streamly
import Streamly.Prelude ((|:))
import Weigh

import qualified Conduit as C
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Streamly.Prelude as S

import Translate (Dictionary, Source, Target, makeTextMap)
import Assets (dictinaries)


main =
    -- memory
    crit
    -- memory2
    -- pathOrEmbedB

makeTextMapS :: Text -> Maybe Dictionary
makeTextMapS txt = HMS.fromListWith
    (\ a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2]) <$> S.toList
        (S.map ((\ (y, x) -> (y :: Source, T.drop 1 x)) . T.span (< '|')) $
            S.fromList $ T.lines txt)

makeTextMapC :: Text -> Dictionary
makeTextMapC txt = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
  (C.runConduitPure
  $ C.yield txt
  .| linesUnbounded
  .| C.mapC (second (T.drop 1) . T.span (<'|'))
  .| C.sinkList)


{-
Case                    Allocated  GCs
base + traverse       396,044,592  284
base + concurrent     396,152,160  284
conduit + traverse    440,853,568  341
conduit + concurrent  440,960,104  341
Benchmark tibet-benchmark: FINISH
-}

memory = do
    dicts <- textLoad
    let toDictMetaT = map makeTextMap
    let toDictMetaC = mapConcurrently (pure . makeTextMap)
    let toDictMetaConduitT = map makeTextMapC
    let toDictMetaConduitC = mapConcurrently (pure . makeTextMapC)
    mainWith $ do
        func "base + traverse" toDictMetaT dicts
        io "base + concurrent" toDictMetaC dicts
        func "conduit + traverse" toDictMetaConduitT dicts
        io "conduit + concurrent" toDictMetaConduitC dicts

{-
benchmarking base + traverse
time                 500.9 ms   (444.2 ms .. 536.9 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 554.4 ms   (530.6 ms .. 600.1 ms)
std dev              45.14 ms   (734.9 μs .. 54.18 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking base + concurrent
time                 513.1 ms   (408.3 ms .. 570.0 ms)
                     0.995 R²   (0.989 R² .. 1.000 R²)
mean                 567.3 ms   (539.4 ms .. 582.6 ms)
std dev              26.65 ms   (8.347 ms .. 35.10 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking conduit + traverse
time                 693.5 ms   (582.3 ms .. 795.2 ms)
                     0.997 R²   (0.988 R² .. 1.000 R²)
mean                 746.6 ms   (720.5 ms .. 777.7 ms)
std dev              35.56 ms   (10.70 ms .. 48.09 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking conduit + concurrent
time                 723.1 ms   (686.5 ms .. 750.0 ms)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 748.3 ms   (735.9 ms .. 762.5 ms)
std dev              14.79 ms   (7.177 ms .. 20.72 ms)
variance introduced by outliers: 19% (moderately inflated)
-}

crit = do
    dicts <- textLoad
    let toDictMetaT = map makeTextMap
    let toDictMetaC = mapConcurrently (pure . makeTextMap)
    let toDictMetaConduitT = map makeTextMapC
    let toDictMetaConduitC = mapConcurrently (pure . makeTextMapC)
    defaultMain
        [ bench "base + traverse" $ nf toDictMetaT dicts
        , bench "base + concurrent" $ nfIO $ toDictMetaC dicts
        , bench "conduit + traverse" $ nf toDictMetaConduitT dicts
        , bench "conduit + concurrent" $ nfIO $ toDictMetaConduitC dicts
        ]

{-
benchmarking embed
time                 16.59 ms   (15.64 ms .. 17.63 ms)
                     0.988 R²   (0.978 R² .. 0.996 R²)
mean                 17.39 ms   (16.62 ms .. 19.94 ms)
std dev              3.071 ms   (830.1 μs .. 5.843 ms)
variance introduced by outliers: 73% (severely inflated)

benchmarking path_hibet
time                 2.691 μs   (2.672 μs .. 2.716 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.680 μs   (2.670 μs .. 2.695 μs)
std dev              40.46 ns   (30.96 ns .. 54.11 ns)
variance introduced by outliers: 14% (moderately inflated)
-}




pathOrEmbedB :: IO ()
pathOrEmbedB = do
  -- let toTextS = map (second TE.decodeUtf8)
  defaultMain
      [ bench "embed" $ nfIO pathLoad
      , bench "path_hibet" $ nf id dictinaries
      ]

pathLoad :: IO [(BS.ByteString, FilePath)]
pathLoad = do
  dir <- getDataFileName "dicts"
  dirAbs <- parseAbsDir dir
  files <- snd <$> listDir dirAbs
  let paths = map fromAbsFile files
  mapM (\p -> (,p) <$> BS.readFile p) paths

textLoad :: IO [Text]
textLoad = do
  dir <- getDataFileName "dicts"
  dirAbs <- parseAbsDir dir
  files <- snd <$> listDir dirAbs
  let paths = map fromAbsFile files
  mapM (\p -> TE.decodeUtf8 <$> BS.readFile p) paths



-- crit2 = do
--     let path = "/media/metaxis/stock/Coding/Haskell/Hibet/dicts/RangjungYeshe-T|E.txt"
--     txt <- T.decodeUtf8 <$> BS.readFile path
--     defaultMain
--         [ bench "usual" $ nf makeTextMap txt
--         , bench "conduit" $ nf makeTextMapC txt
--         , bench "streamly" $ nf makeTextMapS txt
--         ]

-- memory2 = do
--     let file = "/media/metaxis/stock/Coding/Haskell/Hibet/dicts/RangjungYeshe-T|E.txt"
--     txt <- T.decodeUtf8 <$> BS.readFile file
--     mainWith $ do
--         func "usual" makeTextMap txt
--         func "conduit" makeTextMapC txt
--         func "streamly" makeTextMapS txt
