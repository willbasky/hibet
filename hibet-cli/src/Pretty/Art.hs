{-# LANGUAGE CPP #-}

-- | Braille art for the query header: the pure geometry from
-- "Pretty.Braille" plus the renderer, which is built only with the
-- @ascii-art@ flag.
module Pretty.Art
    ( brailleRows
    , fitPpem
    , renderQuery
    )
where

import Data.Text (Text)
import Pretty.Braille (brailleRows, fitPpem)

-- The disabled regions hold the preprocessor lines, which fourmolu cannot
-- parse; everything between them is formatted as usual.
{- FOURMOLU_DISABLE -}
#ifdef ASCII_ART
{- FOURMOLU_ENABLE -}

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_)
import Data.Array.IO (IOUArray, getElems, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Text.Glyphize
import qualified Data.Text.Lazy as TL
import Data.Word (Word32, Word8)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import FreeType
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Draw @query@ as Braille art no wider than @width@ and no taller than a
-- third of @height@. 'Nothing' when there is no Tibetan font or the art would
-- be too small to read.
renderQuery :: Int -> Int -> Text -> IO (Maybe [Text])
renderQuery width height query = do
    mFont <- findFont
    case mFont of
        Nothing -> pure Nothing
        Just path -> drawQuery width height query path

-- | Rows of Braille art to aim for: a third of the terminal height, or
-- @HIBET_ART_ROWS@ while the size is being tuned.
artRows :: Int -> IO Int
artRows height = do
    override <- lookupEnv "HIBET_ART_ROWS"
    pure $ case override >>= readMaybe of
        Just rows | rows > 0 -> rows
        _ -> height `div` 3

-- | Ink bounds of the shaped glyphs in font units: left, right, top, bottom,
-- with y growing up. The line extents span several ems of empty space, so the
-- art is fitted to what the glyphs actually draw.
data Ink = Ink
    { left :: Double
    , right :: Double
    , top :: Double
    , bottom :: Double
    }

drawQuery :: Int -> Int -> Text -> FilePath -> IO (Maybe [Text])
drawQuery width height query path = do
    font <- BS.readFile path
    let face = createFace font 0
        hbFont = createFont face
        upem = fromIntegral (faceUpem face) :: Double
        buffer =
            defaultBuffer
                { text = TL.fromStrict query
                , script = Just "Tibt"
                , direction = Just DirLTR
                }
        glyphs = shape hbFont buffer []
    rows <- artRows height
    case inkBounds hbFont glyphs of
        Nothing -> pure Nothing
        Just ink ->
            case fitPpem
                ((ink.right - ink.left) / upem)
                ((ink.top - ink.bottom) / upem)
                -- one cell less than the budget: a braille cell is 4x8 px
                -- and the ink may start anywhere inside the first one.
                (max 40 ((width - 5) * 4))
                (max 16 ((rows - 1) * 8)) of
                Nothing -> pure Nothing
                Just ppem -> rasterize path upem glyphs ink ppem >>= nonEmpty
    where
        nonEmpty [] = pure Nothing
        nonEmpty rows = pure (Just rows)

inkBounds :: Font -> [(GlyphInfo, GlyphPos)] -> Maybe Ink
inkBounds hbFont glyphs =
    case mapMaybe bounds (zip (scanl advance 0 glyphs) glyphs) of
        [] -> Nothing
        boxes ->
            Just
                Ink
                    { left = minimum [ink.left | ink <- boxes]
                    , right = maximum [ink.right | ink <- boxes]
                    , top = maximum [ink.top | ink <- boxes]
                    , bottom = minimum [ink.bottom | ink <- boxes]
                    }
    where
        advance pen (_, pos) = pen + fromIntegral (x_advance pos)

        bounds (pen, (glyph, pos)) = do
            extents <- fontGlyphExtents hbFont (codepoint glyph)
            let x0 = pen + fromIntegral (x_offset pos) + signed extents.xBearing
                y1 = fromIntegral (y_offset pos) + signed extents.yBearing
            pure
                Ink
                    { left = x0
                    , right = x0 + signed extents.width
                    , top = y1
                    , bottom = y1 + signed extents.height
                    }

-- | harfbuzz-pure reads these as 'Word32', but HarfBuzz stores signed font
-- units, and glyphs may reach below the baseline or left of the origin.
signed :: Word32 -> Double
signed value
    | value > 0x7FFFFFFF = fromIntegral value - 0x100000000
    | otherwise = fromIntegral value

rasterize ::
    FilePath -> Double -> [(GlyphInfo, GlyphPos)] -> Ink -> Int -> IO [Text]
rasterize path upem glyphs ink ppem =
    ft_With_FreeType $ \library ->
        ft_With_Face library path 0 $ \face -> do
            ft_Set_Char_Size face 0 (fromIntegral (ppem * 64)) 0 0
            let pad = 4
                scale = fromIntegral ppem / upem
                originX = fromIntegral pad - ink.left * scale
                canvasWidth = ceiling ((ink.right - ink.left) * scale) + 2 * pad
                canvasHeight = ceiling ((ink.top - ink.bottom) * scale) + 2 * pad
                baseline = pad + ceiling (ink.top * scale)
            canvas <-
                newArray (0, canvasWidth * canvasHeight - 1) 0 ::
                    IO (IOUArray Int Word8)
            drawGlyphs
                face
                canvas
                canvasWidth
                canvasHeight
                baseline
                scale
                originX
                glyphs
            brailleRows canvasWidth canvasHeight <$> getElems canvas

drawGlyphs ::
    FT_Face
    -> IOUArray Int Word8
    -> Int
    -> Int
    -> Int
    -> Double
    -> Double
    -> [(GlyphInfo, GlyphPos)]
    -> IO ()
drawGlyphs _ _ _ _ _ _ _ [] = pure ()
drawGlyphs face canvas width height baseline scale pen ((glyph, pos) : rest) = do
    ft_Load_Glyph face (codepoint glyph) FT_LOAD_RENDER
    slot <- peek =<< frGlyph <$> peek face
    let bitmap = gsrBitmap slot
        bitmapWidth = fromIntegral (bWidth bitmap) :: Int
        bitmapHeight = fromIntegral (bRows bitmap) :: Int
        pitch = fromIntegral (bPitch bitmap) :: Int
        left =
            round (pen + fromIntegral (x_offset pos) * scale)
                + fromIntegral (gsrBitmap_left slot)
        top =
            baseline
                - fromIntegral (gsrBitmap_top slot)
                - round (fromIntegral (y_offset pos) * scale)
    pixels <- peekArray (pitch * bitmapHeight) (bBuffer bitmap)
    let buffer = listArray (0, pitch * bitmapHeight - 1) pixels :: UArray Int Word8
    forM_ [0 .. bitmapHeight - 1] $ \row ->
        forM_ [0 .. bitmapWidth - 1] $ \col -> do
            let x = left + col
                y = top + row
            if x >= 0 && x < width && y >= 0 && y < height
                then do
                    let value = buffer ! (row * pitch + col)
                    old <- readArray canvas (y * width + x)
                    if value > old then writeArray canvas (y * width + x) value else pure ()
                else pure ()
    drawGlyphs
        face
        canvas
        width
        height
        baseline
        scale
        (pen + fromIntegral (x_advance pos) * scale)
        rest

-- | Font path: @HIBET_TIBETAN_FONT@, then fontconfig, then the usual font
-- directories. A candidate counts only when it carries a Tibetan glyph.
findFont :: IO (Maybe FilePath)
findFont = do
    fromEnv <- maybe [] pure <$> lookupEnv "HIBET_TIBETAN_FONT"
    found <- firstTibetan fromEnv
    case found of
        Just path -> pure (Just path)
        Nothing -> do
            matched <- firstTibetan =<< fcMatch
            case matched of
                Just path -> pure (Just path)
                Nothing -> do
                    home <- getHomeDirectory
                    firstTibetan . concat
                        =<< mapM
                            tibetanFonts
                            [ "/usr/share/fonts"
                            , "/usr/local/share/fonts"
                            , home </> ".local/share/fonts"
                            ]

firstTibetan :: [FilePath] -> IO (Maybe FilePath)
firstTibetan [] = pure Nothing
firstTibetan (path : rest) = do
    ok <- isTibetanFont path
    if ok then pure (Just path) else firstTibetan rest

isTibetanFont :: FilePath -> IO Bool
isTibetanFont path = do
    bytes <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    pure $ case bytes of
        Left _ -> False
        Right font ->
            let face = createFace font 0
             in faceUpem face > 0
                    && maybe False (const True) (fontNominalGlyph (createFont face) 'ཀ')

fcMatch :: IO [FilePath]
fcMatch = do
    result <-
        try (readProcess "fc-match" ["-f", "%{file}\\n", "Noto Serif Tibetan"] "") ::
            IO (Either IOException String)
    pure $ case result of
        Left _ -> []
        Right output -> filter (not . null) (lines output)

tibetanFonts :: FilePath -> IO [FilePath]
tibetanFonts dir = do
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure []
        else do
            entries <- listDirectory dir
            fmap concat . forM entries $ \entry -> do
                sub <- doesDirectoryExist (dir </> entry)
                if sub
                    then tibetanFonts (dir </> entry)
                    else
                        pure
                            [ dir </> entry
                            | "tibet" `isInfixOf` map toLower entry
                            , takeExtension entry `elem` [".ttf", ".otf", ".ttc"]
                            ]

{- FOURMOLU_DISABLE -}
#else
{- FOURMOLU_ENABLE -}

-- | The renderer is not built, so the header stays plain text.
renderQuery :: Int -> Int -> Text -> IO (Maybe [Text])
renderQuery _ _ _ = pure Nothing

{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}
