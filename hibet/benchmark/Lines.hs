module Lines
  (linesDict) where

import Common ( textLoad )

-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lines as Line
import Criterion.Main ( defaultMain, bench, nf )

linesDict :: IO ()
linesDict = do
    dicts <- textLoad
    let tl = map Line.fromText dicts
    -- let
    defaultMain
        [ bench "T.lines dicts" $ nf (map T.lines) dicts
        , bench "Lines.Lines dicts " $
            nf (map (Line.lines . Line.fromText)) dicts
        , bench "Lines.Lines text lined dicts " $ nf (map Line.lines ) tl
        ]

{-
Benchmark hibet-benchmark: RUNNING...
benchmarking T.lines dicts
time                 88.18 ms   (40.90 ms .. 120.7 ms)
                     0.798 R²   (0.416 R² .. 0.985 R²)
mean                 112.7 ms   (98.90 ms .. 145.9 ms)
std dev              30.58 ms   (13.92 ms .. 48.55 ms)
variance introduced by outliers: 76% (severely inflated)

benchmarking Lines.Lines dicts
time                 66.11 ms   (62.67 ms .. 69.81 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 69.09 ms   (67.76 ms .. 70.10 ms)
std dev              2.145 ms   (1.524 ms .. 3.076 ms)

benchmarking Lines.Lines text lined dicts
time                 41.69 ms   (38.09 ms .. 46.32 ms)
                     0.977 R²   (0.959 R² .. 0.996 R²)
mean                 39.98 ms   (38.94 ms .. 41.20 ms)
std dev              2.617 ms   (1.851 ms .. 3.810 ms)
variance introduced by outliers: 19% (moderately inflated)

Benchmark hibet-benchmark: FINISH
-}
