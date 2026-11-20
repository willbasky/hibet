{-# LANGUAGE CPP #-}

-- | Braille art for the query header: the pure geometry from
-- "Render.Braille" plus the renderer, which is built only with the
-- @ascii-art@ flag.
module Render.Art
    ( artLines
    , brailleRows
    , findFont
    , fitHeight
    , renderQueryBlocks
    , warnIfNoFont
    )
where

import Data.Text (Text)
import Render.Braille (brailleRows, fitHeight)

-- The disabled regions hold the preprocessor lines, which fourmolu cannot
-- parse; everything between them is formatted as usual.
{- FOURMOLU_DISABLE -}
#ifdef ASCII_ART
{- FOURMOLU_ENABLE -}

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, join)
import Data.Array.IO (IOUArray, getElems, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Glyphize
import qualified Data.Text.Lazy as TL
import Data.Word (Word32, Word8)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import FreeType
import Render.Wrap (syllables)
import System.Directory
    ( XdgDirectory (XdgData)
    , doesDirectoryExist
    , getHomeDirectory
    , getXdgDirectory
    , listDirectory
    )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import System.IO (stderr)
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Draw @query@ as blocks no wider than @width - 4@ cells. Syllables are
-- added greedily until the drawn art no longer fits, then the block closes; a
-- syllable wider by itself is kept whole. The ppem comes from the reference
-- stack, so every block has the same scale.
renderQueryBlocks :: Maybe FilePath -> Int -> Int -> Text -> IO (Maybe [[Text]])
renderQueryBlocks dataDir width lines' query = do
    blocks <-
        withQueryFont dataDir lines' $ \ftFace upem hbFont ppem ->
            drawBlocks
                (width - 4)
                (drawChunk ftFace upem hbFont ppem)
                (syllables query)
    pure (join blocks)

-- | Font lookup, face loading and the ppem shared by every block of a header.
withQueryFont ::
    Maybe FilePath ->
    Int ->
    (FT_Face -> Double -> Font -> Int -> IO a) ->
    IO (Maybe a)
withQueryFont dataDir target action = do
    mFont <- findFont dataDir
    case mFont of
        Nothing -> pure Nothing
        Just path -> do
            font <- BS.readFile path
            let face = createFace font 0
                hbFont = createFont face
                upem = fromIntegral (faceUpem face) :: Double
                referenceGlyphs = shapeTibt hbFont reference
            case inkBounds hbFont referenceGlyphs of
                Nothing -> pure Nothing
                Just referenceInk ->
                    ft_With_FreeType $ \library ->
                        ft_With_Face library path 0 $ \ftFace -> do
                            mPpem <-
                                scaleFor
                                    target
                                    (rasterizeAt ftFace upem referenceGlyphs referenceInk)
                                    referenceInk
                                    upem
                            traverse
                                (\ppem -> action ftFace upem hbFont ppem)
                                mPpem

-- | Greedy blocks of syllables. A syllable wider than the line is kept whole
-- on a line of its own; 'Nothing' when there is no ink at all.
drawBlocks ::
    Int ->
    (Text -> IO [Text]) ->
    [Text] ->
    IO (Maybe [[Text]])
drawBlocks limit draw = go [] "" []
  where
    go done _ rows [] = pure (nonEmpty (reverse (add rows done)))
    go done current rows (syllable : rest) = do
        candidate <- draw (current <> syllable)
        if T.null current || all ((<= limit) . T.length) candidate
            then go done (current <> syllable) candidate rest
            else go (add rows done) "" [] (syllable : rest)

    add rows done
        | null rows = done
        | otherwise = rows : done

    nonEmpty [] = Nothing
    nonEmpty rows = Just rows

-- | Ink of @text@ at @ppem@ as Braille rows; text with no ink gives no rows.
drawChunk :: FT_Face -> Double -> Font -> Int -> Text -> IO [Text]
drawChunk ftFace upem hbFont ppem text =
    case inkBounds hbFont glyphs of
        Nothing -> pure []
        Just ink -> rasterizeAt ftFace upem glyphs ink ppem
  where
    glyphs = shapeTibt hbFont text

-- | How many text lines tall the reference one-layer stack should be: the named
-- environment variable when it holds a positive number, @fallback@ otherwise.
-- The line height is the cell of the console font, so this is the proportion
-- between the art and the text and does not change when the window is resized.
artLines :: String -> Int -> IO Int
artLines variable fallback = do
    override <- lookupEnv variable
    pure $ case override >>= readMaybe of
        Just lines' | lines' > 0 -> lines'
        _ -> fallback

-- | The one-layer stack that sets the scale of the art.
reference :: Text
reference = "མ"

-- | Ink bounds of the shaped glyphs in font units: left, right, top, bottom,
-- with y growing up. The line extents span several ems of empty space, so the
-- art is fitted to what the glyphs actually draw.
data Ink = Ink
    { left :: Double
    , right :: Double
    , top :: Double
    , bottom :: Double
    }

shapeTibt :: Font -> Text -> [(GlyphInfo, GlyphPos)]
shapeTibt hbFont text =
    shape hbFont buffer []
    where
        buffer =
            defaultBuffer
                { text = TL.fromStrict text
                , script = Just "Tibt"
                , direction = Just DirLTR
                }

-- | ppem at which the reference stack is exactly @target@ text lines tall.
-- Everything else is drawn at that scale, so the rectangle floats with the
-- height of the stack: the reference stays the default, a taller stack grows.
scaleFor ::
    Int -> (Int -> IO [Text]) -> Ink -> Double -> IO (Maybe Int)
scaleFor target renderReference referenceInk upem =
    case fitHeight heightPerPpem (max 8 (target * 8)) of
        Nothing -> pure Nothing
        Just nominal -> do
            exact <- firstExact (candidates nominal heightPerPpem)
            pure (Just (maybe nominal id exact))
    where
        heightPerPpem = (referenceInk.top - referenceInk.bottom) / upem

        -- One row of Braille is 8 pixels; the outline height only gets close
        -- to the budget, and a thin edge can fall below the dot threshold and
        -- shave a row off. So the ppem is picked by the cells that are
        -- actually drawn, starting from the height-based estimate.
        firstExact [] = pure Nothing
        firstExact (ppem : rest)
            | ppem < 8 = firstExact rest
            | otherwise = do
                rows <- renderReference ppem
                if length rows == target
                    then pure (Just ppem)
                    else firstExact rest

-- | ppem candidates around the height estimate, nearest first. One row of
-- Braille is 8 pixels, so @8 / heightPerPpem@ is roughly the step that adds a
-- row; a few more on each side cover a row lost to a thin edge.
candidates :: Int -> Double -> [Int]
candidates nominal heightPerPpem =
    [ nominal + offset
    | step <- [0 .. stepCount]
    , offset <- if step == 0 then [0] else [-step, step]
    ]
    where
        stepCount = ceiling (8 / max 0.01 heightPerPpem) + 4

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

-- | Draw the ink at @ppem@ into a canvas and fold it into Braille cells.
-- Leave one empty cell around the ink, so 'brailleRows' can crop it back to a
-- whole number of cells: a cell is 4x8 pixels, so the horizontal pad is a full
-- cell and the vertical one keeps the ink off a cell boundary.
rasterizeAt ::
    FT_Face -> Double -> [(GlyphInfo, GlyphPos)] -> Ink -> Int -> IO [Text]
rasterizeAt face upem glyphs ink ppem = do
    ft_Set_Char_Size face 0 (fromIntegral (ppem * 64)) 0 0
    let padX = 4
        padY = 8
        scale = fromIntegral ppem / upem
        originX = fromIntegral padX - ink.left * scale
        canvasWidth = ceiling ((ink.right - ink.left) * scale) + 2 * padX
        canvasHeight = ceiling ((ink.top - ink.bottom) * scale) + 2 * padY
        baseline = padY + ceiling (ink.top * scale)
    canvas <-
        newArray (0, canvasWidth * canvasHeight - 1) 0 ::
            IO (IOUArray Int Word8)
    let target =
            GlyphTarget
                { face = face
                , canvas = canvas
                , width = canvasWidth
                , height = canvasHeight
                , baseline = baseline
                , scale = scale
                }
    drawGlyphs target originX glyphs
    brailleRows canvasWidth canvasHeight <$> getElems canvas

-- | What stays fixed while the glyphs are drawn: the face, the canvas it
-- fills, and the geometry that maps font units onto the canvas.
data GlyphTarget = GlyphTarget
    { face :: FT_Face
    , canvas :: IOUArray Int Word8
    , width :: Int
    , height :: Int
    , baseline :: Int
    , scale :: Double
    }

drawGlyphs :: GlyphTarget -> Double -> [(GlyphInfo, GlyphPos)] -> IO ()
drawGlyphs _ _ [] = pure ()
drawGlyphs target pen ((glyph, pos) : rest) = do
    ft_Load_Glyph
        target.face
        (codepoint glyph)
        FT_LOAD_RENDER
    slot <- peek =<< frGlyph <$> peek target.face
    let bitmap = gsrBitmap slot
        bitmapWidth = fromIntegral (bWidth bitmap) :: Int
        bitmapHeight = fromIntegral (bRows bitmap) :: Int
        pitch = fromIntegral (bPitch bitmap) :: Int
        left =
            round (pen + fromIntegral (x_offset pos) * target.scale)
                + fromIntegral (gsrBitmap_left slot)
        top =
            target.baseline
                - fromIntegral (gsrBitmap_top slot)
                - round (fromIntegral (y_offset pos) * target.scale)
    bitmapPixels <- peekArray (pitch * bitmapHeight) (bBuffer bitmap)
    let buffer =
            listArray (0, pitch * bitmapHeight - 1) bitmapPixels :: UArray Int Word8
    forM_ [0 .. bitmapHeight - 1] $ \row ->
        forM_ [0 .. bitmapWidth - 1] $ \col -> do
            let x = left + col
                y = top + row
            if x >= 0 && x < target.width && y >= 0 && y < target.height
                then do
                    let value = buffer ! (row * pitch + col)
                    old <- readArray target.canvas (y * target.width + x)
                    if value > old
                        then writeArray target.canvas (y * target.width + x) value
                        else pure ()
                else pure ()
    drawGlyphs
        target
        (pen + fromIntegral (x_advance pos) * target.scale)
        rest

-- | Font path: @HIBET_TIBETAN_FONT@, then the font under the data root
-- (@--data-dir@, then @HIBET_DATA_DIR@, then @$XDG_DATA_HOME/hibet/data@),
-- then fontconfig and the usual font directories. A candidate counts only
-- when it carries a Tibetan glyph; a missing data font falls through to the
-- system search.
findFont :: Maybe FilePath -> IO (Maybe FilePath)
findFont dataDir = do
    fromEnv <- maybe [] pure <$> lookupEnv "HIBET_TIBETAN_FONT"
    fromData <- dataFontFiles dataDir
    found <- firstTibetan (fromEnv ++ fromData)
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

-- | The data set font, in the same order the environment and the XDG layout
-- use. The file is checked by 'isTibetanFont' later.
dataFontFiles :: Maybe FilePath -> IO [FilePath]
dataFontFiles explicit = do
    fromEnv <- lookupEnv "HIBET_DATA_DIR"
    xdg <- getXdgDirectory XdgData "hibet"
    let roots =
            maybe [] pure explicit
                <> maybe [] pure (fromEnv >>= nonEmpty)
                <> [xdg </> "data"]
    pure [root </> "fonts" </> "BabelStoneTibetan.ttf" | root <- roots]
  where
    nonEmpty "" = Nothing
    nonEmpty root = Just root

-- | Warn when no font is available at all, so a data set without the font
-- does not lose the art silently.
warnIfNoFont :: Maybe FilePath -> IO ()
warnIfNoFont dataDir = do
    font <- findFont dataDir
    case font of
        Just _ -> pure ()
        Nothing ->
            TIO.hPutStrLn
                stderr
                "no Tibetan font: Braille art is off; run `hibet data update` or set HIBET_TIBETAN_FONT"

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

-- | The renderer is not built, so only the default art height is known.
artLines :: String -> Int -> IO Int
artLines _ fallback = pure fallback

-- | The renderer is not built, so no font is looked up.
findFont :: Maybe FilePath -> IO (Maybe FilePath)
findFont _ = pure Nothing

-- | The renderer is not built, so there is nothing to warn about.
warnIfNoFont :: Maybe FilePath -> IO ()
warnIfNoFont _ = pure ()

-- | The renderer is not built, so there are no blocks to draw.
renderQueryBlocks :: Maybe FilePath -> Int -> Int -> Text -> IO (Maybe [[Text]])
renderQueryBlocks _ _ _ _ = pure Nothing

{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}
