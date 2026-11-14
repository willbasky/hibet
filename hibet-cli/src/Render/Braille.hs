-- | Pure geometry of the Braille art: a pixel canvas folded into cells and the
-- font size that keeps the art inside the terminal.
module Render.Braille
    ( brailleRows
    , fitPpem
    )
where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

-- | Largest ppem whose ink stays within the pixel budget; 'Nothing' below
-- 8 px, where the art stops reading. Width and height are pixels per ppem.
fitPpem :: Double -> Double -> Int -> Int -> Maybe Int
fitPpem widthPerPpem heightPerPpem maxWidth maxHeight
    | widthPerPpem <= 0 || heightPerPpem <= 0 = Nothing
    | ppem < 8 = Nothing
    | otherwise = Just ppem
    where
        ppem =
            floor
                ( min
                    (fromIntegral maxWidth / widthPerPpem)
                    (fromIntegral maxHeight / heightPerPpem)
                )

-- | Braille art for a canvas of @width@ by @height@ grey pixels. One cell is
-- a 2×4 dot grid, one dot is a 2×2 pixel block, and a dot is inked when more
-- than half of its block is. Empty borders are cropped.
brailleRows :: Int -> Int -> [Word8] -> [Text]
brailleRows width height pixels
    | width < 4 || height < 8 = []
    | null used = []
    | otherwise =
        [ T.pack [cells !! row !! col | col <- [minimum cols' .. maximum cols']]
        | row <- [minimum rowNos .. maximum rowNos]
        ]
    where
        pixel = listArray (0, width * height - 1) pixels :: UArray Int Word8
        cols = width `div` 4
        rows = height `div` 8
        cells = [[cell row col | col <- [0 .. cols - 1]] | row <- [0 .. rows - 1]]
        used =
            [ (row, col)
            | row <- [0 .. rows - 1]
            , col <- [0 .. cols - 1]
            , cells !! row !! col /= ' '
            ]
        rowNos = map fst used
        cols' = map snd used

        cell row col
            | bits == 0 = ' '
            | otherwise = chr (0x2800 + bits)
            where
                bits =
                    sum
                        [ bit
                        | (dx, dy, bit) <- dots
                        , covered (col * 4 + dx * 2) (row * 8 + dy * 2)
                        ]
                covered x0 y0 =
                    let block =
                            [ fromIntegral (pixel ! (y * width + x))
                            | y <- [y0 .. y0 + 1]
                            , x <- [x0 .. x0 + 1]
                            ]
                     in sum block * 2 > 255 * length block

-- | Dot offsets inside a cell: column, row, bit.
dots :: [(Int, Int, Int)]
dots =
    [ (0, 0, 0x01)
    , (0, 1, 0x02)
    , (0, 2, 0x04)
    , (0, 3, 0x40)
    , (1, 0, 0x08)
    , (1, 1, 0x10)
    , (1, 2, 0x20)
    , (1, 3, 0x80)
    ]
