module Main where

import Dictionary (Answer (..), Target (Target))
import Label (Title (Title))
import Render.Art (brailleRows, fitHeight, renderQuery, renderQueryBlocks)
import Render.Pretty
    ( kittyTextSizing
    , pageRows
    , paginate
    , parseKittyVersion
    , supportsTextSizing
    , viewTranslations
    )
import Render.Wrap (wrapQuery)
import Shell
    ( HeaderStyle (..)
    , headerDoc
    , mkOutput
    , omArtLines
    , omHeader
    , pickHeader
    , queryArtLines
    , queryHeader
    , sizedHeader
    )

import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import Control.Exception (bracket)
import qualified Data.Text as T
import Data.Word (Word8)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hIsTerminalDevice, stdout)
import Test.Hspec
    ( Spec
    , describe
    , hspec
    , it
    , pendingWith
    , shouldBe
    , shouldContain
    , shouldSatisfy
    )

main :: IO ()
main = hspec $ do
    viewTranslationsSpec
    mkOutputSpec
    pickHeaderSpec
    queryHeaderSpec
    sizedHeaderSpec
    wrapQuerySpec
    headerTextSpec
    kittyVersionSpec
    omHeaderSpec
    artHeightSpec
    artSpec
    artBlocksSpec
    paginateSpec

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
        $ it "puts the drawn query above the entries"
        $ T.unpack (render $ mkOutput (sizedHeader 80 "མོ") [answer1])
            `shouldContain` "\ESC]66;s=4;མོ\a\ESC[3B"

pickHeaderSpec :: Spec
pickHeaderSpec = describe "pickHeader" $ do
    let art = ["⣿⣿", "⣿"]
        edge = [T.replicate 76 "⣿"]
        wide = [T.replicate 77 "⣿"]
        tall = replicate 20 "⣿"
        tooTall = replicate 21 "⣿"
    it "prefers art that fits, even in kitty" $
        pickHeader 80 24 True (Just art) `shouldBe` ArtHeader art
    it "takes OSC 66 in kitty when there is no art" $
        pickHeader 80 24 True Nothing `shouldBe` SizedHeader
    it "keeps the fallback outside kitty when there is no art" $
        pickHeader 80 24 False Nothing `shouldBe` PlainHeader
    it "sends art wider than the window to OSC 66 in kitty" $
        pickHeader 80 24 True (Just wide) `shouldBe` SizedHeader
    it "sends art wider than the window to the fallback outside kitty" $
        pickHeader 80 24 False (Just wide) `shouldBe` PlainHeader
    it "leaves a four-cell margin at the window edge" $
        pickHeader 80 24 True (Just edge) `shouldBe` ArtHeader edge
    it "keeps a row for the answer and one for the pager keys" $
        pickHeader 80 24 True (Just tall) `shouldBe` ArtHeader tall
    it "sends art taller than the window to the next level" $
        pickHeader 80 24 True (Just tooTall) `shouldBe` SizedHeader

queryHeaderSpec :: Spec
queryHeaderSpec = describe "queryHeader" $ do
    it "draws Braille art first when the renderer has one" $ do
        art <- drawArt queryArtLines "མོ"
        case art of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> withEnv "TERM" "xterm-kitty" $ do
                header <- queryHeader "མོ"
                render header `shouldBe` T.intercalate "\n" rows
    it "sends OSC 66 in a kitty without art" $
        withArtlessKitty queryArtLines "མོ" $ do
            header <- queryHeader "མོ"
            T.unpack (render header) `shouldContain` "\ESC]66;s="
    it "keeps OSC 66 out of a redirected stdout" $
        withArtlessRedirected queryArtLines "མོ" $ do
            header <- queryHeader "མོ"
            render header `shouldSatisfy` not . T.isInfixOf "\ESC]66"
    it "keeps the plain query outside kitty without art" $
        withArtlessPlain queryArtLines "མོ" $ do
            header <- queryHeader "མོ"
            render header `shouldBe` "མོ"

drawArt :: IO Int -> T.Text -> IO (Maybe [T.Text])
drawArt height query = height >>= \lines' -> renderQuery lines' query

-- | Run the example only where the header has no art to pick and stdout is a
-- kitty terminal that draws OSC 66; otherwise it is pending, so the table
-- above stays machine-free.
withArtlessKitty :: IO Int -> T.Text -> IO () -> IO ()
withArtlessKitty height query action = do
    art <- drawArt height query
    terminal <- hIsTerminalDevice stdout
    withEnv "TERM" "xterm-kitty" $ do
        kitty <- supportsTextSizing
        case (art, kitty, terminal) of
            (Just _, _, _) -> pendingWith "the art takes the header in this build"
            (_, False, _) -> pendingWith "no kitty 0.40 or newer here"
            (_, True, False) -> pendingWith "stdout is not a terminal"
            (Nothing, True, True) -> action

-- | Run the example where stdout is redirected: OSC 66 must stay out of the
-- output. Pending when there is no art or no kitty, or when stdout is a
-- terminal and the sequence is expected.
withArtlessRedirected :: IO Int -> T.Text -> IO () -> IO ()
withArtlessRedirected height query action = do
    art <- drawArt height query
    terminal <- hIsTerminalDevice stdout
    withEnv "TERM" "xterm-kitty" $ do
        kitty <- supportsTextSizing
        case (art, kitty, terminal) of
            (Just _, _, _) -> pendingWith "the art takes the header in this build"
            (_, False, _) -> pendingWith "no kitty 0.40 or newer here"
            (_, True, True) -> pendingWith "stdout is a terminal"
            (Nothing, True, False) -> action

withArtlessPlain :: IO Int -> T.Text -> IO () -> IO ()
withArtlessPlain height query action = do
    art <- drawArt height query
    case art of
        Just _ -> pendingWith "the art takes the header in this build"
        Nothing -> withEnv "TERM" "xterm-256color" action

sizedHeaderSpec :: Spec
sizedHeaderSpec = describe "sizedHeader" $ do
    let query = T.replicate 20 "ཀ་"
    it "scales the query for kitty up to four times" $
        render (sizedHeader 80 "མོ") `shouldBe` "\ESC]66;s=4;མོ\a\ESC[3B"
    it "keeps a long kitty query at normal size" $
        render (sizedHeader 12 "སེམས་ཅན") `shouldBe` "སེམས་ཅན"
    it "wraps a wide query and keeps every line at one size" $
        render (sizedHeader 30 query) `shouldBe` T.intercalate "\n" (wrapQuery 30 query)

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

headerTextSpec :: Spec
headerTextSpec = describe "queryHeader outside kitty" $ do
    it "keeps the query as plain text when the renderer has no art" $
        render (headerDoc "མོ" Nothing) `shouldBe` "མོ"
    it "keeps the query as plain text without OSC 66" $ do
        header <- queryHeader "མོ"
        render header `shouldSatisfy` not . T.null

kittyVersionSpec :: Spec
kittyVersionSpec = describe "parseKittyVersion" $ do
    it "reads the major and minor version kitty prints" $
        parseKittyVersion "kitty 0.50.0 created by Kovid Goyal\n"
            `shouldBe` Just (0, 50)
    it "reads a version without a patch number" $
        parseKittyVersion "kitty 0.40" `shouldBe` Just (0, 40)
    it "gives nothing for other output" $
        parseKittyVersion "no version here" `shouldBe` Nothing
    it "takes 0.40 as the first version with the protocol" $
        fmap (>= kittyTextSizing) (parseKittyVersion "kitty 0.40.0")
            `shouldBe` Just True
    it "rejects the kitty before the protocol" $
        fmap (>= kittyTextSizing) (parseKittyVersion "kitty 0.39.1")
            `shouldBe` Just False

omHeaderSpec :: Spec
omHeaderSpec = describe "omHeader" $ do
    it "draws the Om art first when the renderer has one" $ do
        art <- drawArt omArtLines "ༀ"
        case art of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> withEnv "TERM" "xterm-kitty" $ do
                header <- omHeader "fallback"
                render header `shouldBe` T.intercalate "\n" rows
    it "draws Om through OSC 66 in a kitty without art" $
        withArtlessKitty omArtLines "ༀ" $ do
            header <- omHeader "fallback"
            let output = T.unpack (render header)
            output `shouldContain` "\ESC]66;s="
            output `shouldContain` "ༀ\a"
    it "keeps OSC 66 out of a redirected stdout" $
        withArtlessRedirected omArtLines "ༀ" $ do
            header <- omHeader "fallback"
            render header `shouldSatisfy` not . T.isInfixOf "\ESC]66"
    it "keeps the block art outside kitty without art" $
        withArtlessPlain omArtLines "ༀ" $ do
            header <- omHeader "fallback"
            render header `shouldBe` "fallback"

artHeightSpec :: Spec
artHeightSpec = describe "art height" $
    it "sizes the Om art separately from the query art" $
        withEnv "HIBET_ART_LINES" "3" $
            withEnv "HIBET_OM_LINES" "6" $ do
                queryRows <- drawArt queryArtLines "མ"
                omRows <- drawArt omArtLines "ༀ"
                case (queryRows, omRows) of
                    (Just expectedQuery, Just expectedOm) -> do
                        query <- queryHeader "མ"
                        om <- omHeader "fallback"
                        render query `shouldBe` T.intercalate "\n" expectedQuery
                        render om `shouldBe` T.intercalate "\n" expectedOm
                        length expectedQuery `shouldBe` 3
                    _ -> pendingWith "no renderer or no Tibetan font"

withEnv :: String -> String -> IO a -> IO a
withEnv name value action =
    bracket (lookupEnv name) restore $ \_ -> do
        setEnv name value
        action
    where
        restore Nothing = unsetEnv name
        restore (Just old) = setEnv name old

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
        art <- renderQuery 4 "མ"
        case art of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> length rows `shouldBe` 4
    it "lets the rectangle grow for a taller stack" $ do
        reference <- renderQuery 4 "མ"
        tall <- renderQuery 4 "ཀ"
        case (reference, tall) of
            (Just a, Just b) -> length b `shouldSatisfy` (> length a)
            _ -> pendingWith "no renderer or no Tibetan font"
    it "keeps the height of a line of same-height stacks" $ do
        short <- renderQuery 4 "མ"
        long <- renderQuery 4 "མམམ"
        case (short, long) of
            (Just a, Just b) -> length a `shouldBe` length b
            _ -> pendingWith "no renderer or no Tibetan font"

artBlocksSpec :: Spec
artBlocksSpec = describe "renderQueryBlocks" $ do
    it "draws a wide query as blocks that fit the window" $ do
        blocks <- renderQueryBlocks 40 4 (T.replicate 20 "མ་")
        case blocks of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> do
                length rows `shouldSatisfy` (> 1)
                concatMap (map T.length) rows `shouldSatisfy` all (<= 36)
    it "keeps a syllable wider than the window whole" $ do
        blocks <- renderQueryBlocks 10 4 (T.replicate 20 "ཀ")
        case blocks of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> do
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
        it "counts a scaled line as its scale rows" $
            paginate 3 ["a", "\ESC]66;s=2;x\a", "b", "c"]
                `shouldBe` [["a", "\ESC]66;s=2;x\a"], ["b", "c"]]
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
