{-# LANGUAGE CPP #-}

module Main where

import Dictionary (Answer (..), Target (Target))
import Label (Title (Title))
import Render.Art (brailleRows, findFont, fitHeight, renderQueryBlocks)
import Render.Keys (Key (..), readKeyWith)
import Render.Pretty
    ( keysRows
    , pageRows
    , paginate
    , pprint
    , viewTranslations
    , withHeaderSpaces
    )
import Render.Wrap (wrapQuery)
import Shell
    ( HeaderStyle (..)
    , farewell
    , farewellHeader
    , mkOutput
    , omArtLines
    , omHeader
    , pickHeader
    , queryArtLines
    , queryHeader
    , queryReply
    )
import Type (HibetError (..), renderError)

import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty, vsep)
import Prettyprinter.Render.Text (renderStrict)

import Control.Exception (IOException, bracket, finally, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAscii)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import System.Directory (doesFileExist)
import System.Environment
    ( getArgs
    , getExecutablePath
    , lookupEnv
    , setEnv
    , unsetEnv
    )
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hClose, hFlush)
import System.Posix.IO (dup, fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , hspec
    , it
    , pendingWith
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldSatisfy
    )

main :: IO ()
main = do
    setDataRoot
    args <- getArgs
    case args of
        ["--pager-child"] -> pagerChild
        _ -> hspec $ do
            viewTranslationsSpec
            mkOutputSpec
            pickHeaderSpec
            queryHeaderSpec
            farewellSpec
            wrapQuerySpec
            omHeaderSpec
            artHeightSpec
            artSpec
            artBlocksSpec
            paginateSpec
            keysRowsSpec
            keySpec
            pagerSpec
            renderErrorSpec
            queryReplySpec

renderErrorSpec :: Spec
renderErrorSpec =
    describe "renderError"
        $ it "prints the summary and the detail without the constructor"
        $ renderError (DataInvalid "titles.toml" "line 1")
            `shouldBe` "Invalid data in titles.toml\nline 1"

queryReplySpec :: Spec
queryReplySpec = describe "queryReply" $ do
    it "shows the reason of a rejected query" $
        queryReply (Left (QueryInvalid "Token parser error"))
            `shouldBe` Left "Invalid query\nToken parser error"
    it "shows Nothing found only for an empty answer" $
        queryReply (Right ("re ba", [])) `shouldBe` Left "Nothing found"
    it "passes a non-empty answer through" $
        queryReply (Right ("re ba", [answer1]))
            `shouldBe` Right ("re ba", [answer1])

viewTranslationsSpec :: Spec
viewTranslationsSpec =
    describe "Elem Text"
        $ it "Success with Test 1"
        $ renderStrict
            (layoutPretty defaultLayoutOptions $ viewTranslations [answer1, answer2])
            `shouldBe` "1. Dict1\n  target\n \n2. Dict2\n  target2"

answer1 :: Answer
answer1 =
    Answer
        { targets = [Target "target"]
        , dictNumber = 1
        , dictTitle = Title "Dict1"
        }

answer2 :: Answer
answer2 =
    Answer
        { targets = [Target "target2"]
        , dictNumber = 2
        , dictTitle = Title "Dict2"
        }

mkOutputSpec :: Spec
mkOutputSpec =
    describe "mkOutput"
        $ it "puts the query above the entries"
        $ let output = T.unpack (render $ mkOutput (pretty ("མོ" :: T.Text)) [answer1])
           in do
                output `shouldContain` "མོ"
                output `shouldContain` "1. Dict1"

pickHeaderSpec :: Spec
pickHeaderSpec = describe "pickHeader" $ do
    let art = ["⣿⣿", "⣿"]
        edge = [T.replicate 76 "⣿"]
        wide = [T.replicate 77 "⣿"]
        tall = replicate 20 "⣿"
        tooTall = replicate 21 "⣿"
    it "prefers art that fits" $
        pickHeader 80 24 (Just art) `shouldBe` ArtHeader art
    it "falls back when there is no art" $
        pickHeader 80 24 Nothing `shouldBe` PlainHeader
    it "falls back when art is wider than the window" $
        pickHeader 80 24 (Just wide) `shouldBe` PlainHeader
    it "leaves a four-cell margin at the window edge" $
        pickHeader 80 24 (Just edge) `shouldBe` ArtHeader edge
    it "keeps a row for the answer and one for the pager keys" $
        pickHeader 80 24 (Just tall) `shouldBe` ArtHeader tall
    it "falls back when art is taller than the window" $
        pickHeader 80 24 (Just tooTall) `shouldBe` PlainHeader

queryHeaderSpec :: Spec
queryHeaderSpec = describe "queryHeader" $ do
    it "draws Braille art first when the renderer has one" $ do
        art <- drawArt queryArtLines "མོ"
        rows <- requireArt "queryHeader" art
        header <- queryHeader Nothing "མོ"
        render header `shouldBe` T.intercalate "\n" rows
    it "keeps the plain query without art" $
        withArtlessPlain queryArtLines "མོ" $ do
            header <- queryHeader Nothing "མོ"
            render header `shouldBe` "མོ"

farewellSpec :: Spec
farewellSpec = describe "farewell" $ do
    it "keeps the goodbye free of English text" $
        T.all (not . isAscii) farewell `shouldBe` True
    it "draws the plain farewell without art" $
        withArtlessPlain queryArtLines farewell $ do
            header <- farewellHeader Nothing
            render header `shouldBe` farewell

-- | Art is mandatory when the renderer and the packaged font are shipped; a
-- build without the renderer has nothing to draw.
requireArt :: String -> Maybe a -> IO a
{- FOURMOLU_DISABLE -}
#ifdef ASCII_ART
{- FOURMOLU_ENABLE -}
requireArt what =
    maybe
        (expectationFailure (what <> ": the bundled font renders no art") >> fail "no art")
        pure
{- FOURMOLU_DISABLE -}
#else
{- FOURMOLU_ENABLE -}
requireArt _ _ = pendingWith "the renderer is not built" >> fail "no renderer"
{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}

-- | Point the renderer at the repository data, which carries the font. The
-- data set is found by walking up from the test executable, so the suite works
-- from @cabal test@ and from a copied binary.
setDataRoot :: IO ()
setDataRoot = do
    exe <- getExecutablePath
    root <-
        firstDataRoot
            [dir </> "hibet-data" | dir <- takeWhile (/= "/") (iterate takeDirectory exe)]
    maybe (pure ()) (setEnv "HIBET_DATA_DIR") root

firstDataRoot :: [FilePath] -> IO (Maybe FilePath)
firstDataRoot [] = pure Nothing
firstDataRoot (root : rest) = do
    exists <- doesFileExist (root </> "fonts" </> "BabelStoneTibetan.ttf")
    if exists then pure (Just root) else firstDataRoot rest

-- | One syllable fits in a single block at 80 columns.
drawArt :: IO Int -> T.Text -> IO (Maybe [T.Text])
drawArt height query = do
    lines' <- height
    fmap concat <$> renderQueryBlocks Nothing 80 lines' query

withArtlessPlain :: IO Int -> T.Text -> IO () -> IO ()
withArtlessPlain height query action = do
    art <- drawArt height query
    case art of
        Just _ -> pendingWith "the art takes the header in this build"
        Nothing -> action

wrapQuerySpec :: Spec
wrapQuerySpec = describe "wrapQuery" $ do
    let query = T.replicate 20 "ཀ་"
    it "breaks a wide query into lines within the margin" $ do
        let rows = wrapQuery 20 query
        length rows `shouldSatisfy` (> 1)
        map T.length rows `shouldSatisfy` all (<= 16)
    it "breaks only after a tsek" $
        all (T.isSuffixOf "་") (init (wrapQuery 20 query)) `shouldBe` True
    it "keeps the query whole" $
        T.concat (wrapQuery 20 query) `shouldBe` query
    it "starts a new line before the syllable that does not fit" $
        wrapQuery 8 "ཀ་ཁ་ག" `shouldBe` ["ཀ་ཁ་", "ག"]
    it "does not cut a syllable wider than the window" $
        wrapQuery 8 (T.replicate 30 "ཀ") `shouldBe` [T.replicate 30 "ཀ"]
    it "keeps a short query on one line" $
        wrapQuery 80 "མོ" `shouldBe` ["མོ"]

omHeaderSpec :: Spec
omHeaderSpec = describe "omHeader" $ do
    it "draws the Om art first when the renderer has one" $ do
        withEnv "HIBET_OM_LINES" "6" $ do
            art <- drawArt omArtLines "ༀ"
            rows <- requireArt "omHeader" art
            header <- omHeader Nothing "fallback"
            render header `shouldBe` T.intercalate "\n" rows
    it "keeps the block art without art" $
        withArtlessPlain omArtLines "ༀ" $ do
            header <- omHeader Nothing "fallback"
            render header `shouldBe` "fallback"

artHeightSpec :: Spec
artHeightSpec = describe "art height" $
    it "sizes the Om art separately from the query art" $
        withEnv "HIBET_ART_LINES" "3" $
            withEnv "HIBET_OM_LINES" "6" $ do
                queryRows <- drawArt queryArtLines "མ"
                omRows <- drawArt omArtLines "ༀ"
                expectedQuery <- requireArt "query art" queryRows
                expectedOm <- requireArt "Om art" omRows
                query <- queryHeader Nothing "མ"
                om <- omHeader Nothing "fallback"
                render query `shouldBe` T.intercalate "\n" expectedQuery
                render om `shouldBe` T.intercalate "\n" expectedOm
                length expectedQuery `shouldBe` 3

withEnv :: String -> String -> IO a -> IO a
withEnv name value action =
    bracket (lookupEnv name) restore $ \_ -> do
        setEnv name value
        action
    where
        restore Nothing = unsetEnv name
        restore (Just old) = setEnv name old

latinFonts :: [FilePath]
latinFonts =
    [ "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    , "/usr/share/fonts/TTF/DejaVuSans.ttf"
    , "/usr/share/fonts/dejavu/DejaVuSans.ttf"
    ]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (path : rest) = do
    exists <- doesFileExist path
    if exists then pure (Just path) else firstExisting rest

artSpec :: Spec
artSpec = describe "Render.Art" $ do
    it "draws a fully covered cell as a full braille glyph" $
        brailleRows 4 8 (replicate 32 255) `shouldBe` ["⣿"]
    it "crops empty borders around the ink" $
        brailleRows 8 16 inkInBottomRightCell `shouldBe` ["⣿"]
    it "picks the largest ppem that fits the height" $
        fitHeight 20 200 `shouldBe` Just 10
    it "gives up when the height is too small" $
        fitHeight 20 60 `shouldBe` Nothing
    it "keeps the default height for the one-layer reference" $ do
        rows <- requireArt "reference" =<< drawArt (pure 4) "མ"
        length rows `shouldBe` 4
    it "lets the rectangle grow for a taller stack" $ do
        reference <- requireArt "reference" =<< drawArt (pure 4) "མ"
        tall <- requireArt "taller stack" =<< drawArt (pure 4) "ཀ"
        length tall `shouldSatisfy` (> length reference)
    it "keeps the height of a line of same-height stacks" $ do
        short <- requireArt "one stack" =<< drawArt (pure 4) "མ"
        long <- requireArt "three stacks" =<< drawArt (pure 4) "མམམ"
        length short `shouldBe` length long

    it "finds the data font before the system" $
        withEnv "HIBET_TIBETAN_FONT" "/nonexistent" $ do
            path <- findFont Nothing
            case path of
                Nothing -> pendingWith "the renderer is not built"
                Just found -> do
                    doesFileExist found `shouldReturn` True
                    found `shouldSatisfy` isInfixOf "BabelStoneTibetan.ttf"

    it "takes the font from an explicit data dir" $ do
        root <- lookupEnv "HIBET_DATA_DIR"
        case root of
            Nothing -> pendingWith "no data directory with the font"
            Just dir -> do
                built <- findFont Nothing
                case built of
                    Nothing -> pendingWith "the renderer is not built"
                    Just _ ->
                        withEnv "HIBET_DATA_DIR" "/nonexistent" $ do
                            path <- findFont (Just dir)
                            path `shouldSatisfy` maybe False (isInfixOf "BabelStoneTibetan.ttf")

    it "ignores an override without a Tibetan glyph" $ do
        latin <- firstExisting latinFonts
        case latin of
            Nothing -> pendingWith "no Latin font to test the override"
            Just path ->
                withEnv "HIBET_TIBETAN_FONT" path $ do
                    rows <- requireArt "non-Tibetan override" =<< drawArt (pure 4) "མ"
                    length rows `shouldBe` 4

artBlocksSpec :: Spec
artBlocksSpec = describe "renderQueryBlocks" $ do
    it "draws a wide query as blocks that fit the window" $ do
        blocks <- renderQueryBlocks Nothing 40 4 (T.replicate 20 "མ་")
        rows <- requireArt "blocks" blocks
        length rows `shouldSatisfy` (> 1)
        concatMap (map T.length) rows `shouldSatisfy` all (<= 36)
    it "keeps a syllable wider than the window whole" $ do
        blocks <- renderQueryBlocks Nothing 10 4 (T.replicate 20 "ཀ")
        rows <- requireArt "wide syllable" blocks
        length rows `shouldBe` 1
        maximum (map T.length (concat rows)) `shouldSatisfy` (> 6)

-- 8×16 pixels: ink only in the bottom-right cell.
inkInBottomRightCell :: [Word8]
inkInBottomRightCell =
    concat (replicate 8 (replicate 8 0))
        <> concat (replicate 8 [0, 0, 0, 0, 255, 255, 255, 255])

render :: Doc ann -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions

paginateSpec :: Spec
paginateSpec =
    describe "paginate" $ do
        it "keeps order and splits by page size" $
            paginate 2 ["a", "b", "c", "d", "e"]
                `shouldBe` [["a", "b"], ["c", "d"], ["e"]]
        it "gives nothing for no lines" $
            paginate 2 [] `shouldBe` []
        it "never loops on a zero page size" $
            paginate 0 ["a"] `shouldBe` [["a"]]
        it "counts every line of a multi-line header" $
            pageRows ["", "query one", "query two", ""] `shouldBe` 4
        it "keeps a page within the window under a multi-line header" $ do
            let header = ["", "query one", "query two", ""]
                pageSize = max 1 (10 - 1 - pageRows header)
            map length (paginate pageSize (replicate 20 "answer"))
                `shouldSatisfy` all (<= pageSize)

keysRowsSpec :: Spec
keysRowsSpec = describe "keysRows" $ do
    it "takes one row for the full hint in a wide window" $
        keysRows 80 `shouldBe` 1
    it "wraps the widest hint in a 40-column window" $
        keysRows 40 `shouldBe` 2
    it "wraps the widest hint in a 20-column window" $
        keysRows 20 `shouldBe` 3
    it "never divides by a zero width" $
        keysRows 0 `shouldSatisfy` (> 0)

keySpec :: Spec
keySpec = describe "readKeyWith" $ mapM_ example keyTable
    where
        example (bytes, key) =
            it (show bytes <> " reads as " <> show key) $
                keyOf bytes `shouldReturn` key

-- | Bytes of one key press and the key the pager acts on.
keyTable :: [(String, Key)]
keyTable =
    [ ("q", Quit)
    , ("\x03", Quit) -- Ctrl-C
    , ("", Quit) -- end of input
    , ("j", LineDown)
    , ("k", LineUp)
    , (" ", PageDown)
    , ("b", PageUp)
    , ("\x02", PageUp) -- Ctrl-B
    , ("\x06", PageDown) -- Ctrl-F
    , ("\x0e", LineDown) -- Ctrl-N
    , ("\x10", LineUp) -- Ctrl-P
    , ("\x16", PageDown) -- Ctrl-V
    , ("\ESCv", PageUp) -- Alt-V
    , ("\ESC[A", LineUp)
    , ("\ESC[B", LineDown)
    , ("\ESCOA", LineUp) -- application cursor keys
    , ("\ESCOB", LineDown)
    , ("\ESC[5~", PageUp)
    , ("\ESC[6~", PageDown)
    , ("\ESC[9~", Other)
    , ("\ESC[5", Other) -- a cut sequence
    , ("\ESC", Other) -- a lone Escape
    , ("x", Other)
    ]

-- | Read one key press from a fixed byte string.
keyOf :: String -> IO Key
keyOf bytes = do
    rest <- newIORef bytes
    readKeyWith $ \_ -> atomicModifyIORef' rest $ \case
        [] -> ([], Nothing)
        (c : cs) -> (cs, Just c)

pagerSpec :: Spec
pagerSpec = describe "paged" $ do
    it "pages on the alternate screen, moves a page and restores it on q" $
        withPtyChild (80, 24) True $ \master -> do
            drawn <- readPty "" master (BS.isInfixOf clearScreen)
            BS.hPut master " " >> hFlush master
            paged <- readPty drawn master (frameDone 2)
            BS.hPut master "\ESC[5~" >> hFlush master
            back <- readPty paged master (frameDone 3)
            BS.hPut master "q" >> hFlush master
            out <- readPty back master (const False)
            case frames back of
                [before, afterSpace, afterUp] -> do
                    before `shouldSatisfy` BS.isInfixOf "L1;"
                    afterSpace `shouldSatisfy` not . BS.isInfixOf "L1;"
                    afterUp `shouldBe` before
                fs -> expectationFailure ("expected three frames, got " <> show (length fs))
            countSub "\ESC[?1049h" out `shouldBe` 1
            countSub "\ESC[?1049l" out `shouldBe` 1
            let afterAlt = snd (BS.breakSubstring "\ESC[?1049l" out)
            afterAlt `shouldSatisfy` BS.isInfixOf "L1;"
            countSub "L100;" afterAlt `shouldBe` 1
            countSub ";" afterAlt `shouldBe` 100
    it "keeps the page inside a narrow window" $
        mapM_ fits [(20, 24), (40, 24)]
    it "keeps the plain output when stdin is not a terminal" $
        withPtyChild (80, 24) False $ \master -> do
            out <- readPty "" master (const False)
            out `shouldSatisfy` not . BS.isInfixOf "\ESC[?1049h"
            countSub "L100;" out `shouldBe` 1
    where
        -- The query stays pinned and the wrapped keys hint stays on screen:
        -- header rows plus the page plus the hint fit the window height.
        fits size@(width, height) =
            withPtyChild size True $ \master -> do
                out <- readPty "" master (frameDone 1)
                case frames out of
                    [frame] -> do
                        frame `shouldSatisfy` BS.isInfixOf "query"
                        frame `shouldSatisfy` BS.isInfixOf "q: quit --"
                        frameRows width frame `shouldSatisfy` (<= height)
                    fs ->
                        expectationFailure
                            ("expected one frame, got " <> show (length fs))

-- | The pager as the shell calls it: the document starts with the header.
pagerChild :: IO ()
pagerChild = pprint header (withHeaderSpaces id header body)
    where
        header = pretty ("query" :: T.Text)
        body =
            vsep
                [ pretty ("L" <> T.pack (show n) <> ";")
                | n <- [1 :: Int .. 100]
                ]

-- | Run this executable as @--pager-child@ with stdout on a fresh pty of the
-- given size and stdin on the same pty when @ttyStdin@ is true, or on
-- @/dev/null@ otherwise. Gives the master end to the example and kills the
-- child when it fails.
withPtyChild :: (Int, Int) -> Bool -> (Handle -> IO a) -> IO a
withPtyChild (width, height) ttyStdin action = do
    (master, slave) <- openPseudoTerminal
    hMaster <- fdToHandle master
    hSlave <- fdToHandle slave
    -- createProcess takes the handle for the child and closes it, so stty gets
    -- its own copy of the same device.
    hSize <- fdToHandle =<< dup slave
    setPtySize hSize width height
    exe <- getExecutablePath
    (_, _, _, child) <-
        createProcess
            (proc exe ["--pager-child"])
                { std_in = if ttyStdin then UseHandle hSlave else NoStream
                , std_out = UseHandle hSlave
                , std_err = Inherit
                }
    hClose hSlave
    action hMaster
        `finally` do
            hClose hMaster
            _ <- try (terminateProcess child) :: IO (Either IOException ())
            _ <- waitForProcess child
            pure ()

-- | Give the pty a real window size: a fresh one reports 0 rows by 0 columns,
-- which no terminal has.
setPtySize :: Handle -> Int -> Int -> IO ()
setPtySize handle width height = do
    (_, _, _, child) <-
        createProcess
            (proc "stty" ["rows", show height, "cols", show width])
                { std_in = UseHandle handle
                , std_out = NoStream
                , std_err = Inherit
                }
    _ <- waitForProcess child
    pure ()

-- | Bytes from the pty until @done@ holds for everything read so far, or the
-- child closes the terminal.
readPty :: ByteString -> Handle -> (ByteString -> Bool) -> IO ByteString
readPty start handle done = go start
    where
        go acc
            | done acc = pure acc
            | otherwise = do
                chunk <-
                    try (timeout ptyWait (BS.hGetSome handle 4096)) ::
                        IO (Either IOException (Maybe ByteString))
                case chunk of
                    Left _ -> pure acc
                    Right Nothing -> expectationFailure "the pager did not answer" >> pure acc
                    Right (Just bytes)
                        | BS.null bytes -> pure acc
                        | otherwise -> go (acc <> bytes)

ptyWait :: Int
ptyWait = 10000000

-- | The pager clears the screen before every frame.
clearScreen :: ByteString
clearScreen = "\ESC[H\ESC[2J"

frames :: ByteString -> [ByteString]
frames = drop 1 . splitOn clearScreen

-- | Rows a frame takes on a @width@-cell screen. The pager prints lines and
-- lets the terminal wrap them, so a long line is more than one row.
frameRows :: Int -> ByteString -> Int
frameRows width frame =
    sum (map rows (T.splitOn "\n" (stripEscapes (TE.decodeUtf8 frame))))
    where
        rows line = max 1 ((T.length line + width - 1) `div` max 1 width)

-- | Drop color sequences: they take no cells.
stripEscapes :: T.Text -> T.Text
stripEscapes text = case T.breakOn "\ESC[" text of
    (before, rest)
        | T.null rest -> before
        | otherwise ->
            let after =
                    T.drop
                        1
                        (T.dropWhile (\c -> c < '@' || c > '~') (T.drop 2 rest))
             in before <> stripEscapes after

-- | The n-th frame has started and the last one is complete: the keys line is
-- drawn after the body.
frameDone :: Int -> ByteString -> Bool
frameDone n bs = case frames bs of
    fs | length fs >= n -> "q: quit --" `BS.isInfixOf` last fs
    _ -> False

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn needle hay = case BS.breakSubstring needle hay of
    (before, rest)
        | BS.null rest -> [before]
        | otherwise -> before : splitOn needle (BS.drop (BS.length needle) rest)

countSub :: ByteString -> ByteString -> Int
countSub needle = go
    where
        go hay = case BS.breakSubstring needle hay of
            (_, rest)
                | BS.null rest -> 0
                | otherwise -> 1 + go (BS.drop (BS.length needle) rest)
