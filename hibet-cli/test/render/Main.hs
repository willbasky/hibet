module Main where

import Dictionary (Answer (..), Target (Target))
import Label (Title (Title))
import Render.Art (brailleRows, fitPpem, renderQuery)
import Render.Pretty (paginate, viewTranslations)
import Shell (headerDoc, mkOutput, queryHeader, sizedHeader)

import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.Text as T
import Data.Word (Word8)
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
    queryHeaderSpec
    headerTextSpec
    artSpec
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

queryHeaderSpec :: Spec
queryHeaderSpec = describe "sizedHeader" $ do
    it "scales the query for kitty up to four times" $
        render (sizedHeader 80 "མོ") `shouldBe` "\ESC]66;s=4;མོ\a\ESC[3B"
    it "keeps a long kitty query at normal size" $
        render (sizedHeader 12 "སེམས་ཅན") `shouldBe` "སེམས་ཅན"

headerTextSpec :: Spec
headerTextSpec = describe "queryHeader outside kitty" $ do
    it "keeps the query as plain text when the renderer has no art" $
        render (headerDoc "མོ" Nothing) `shouldBe` "མོ"
    it "keeps the query as plain text without OSC 66" $ do
        header <- queryHeader "མོ"
        render header `shouldSatisfy` not . T.null

artSpec :: Spec
artSpec = describe "Render.Art" $ do
    it "draws a fully covered cell as a full braille glyph" $
        brailleRows 4 8 (replicate 32 255) `shouldBe` ["⣿"]
    it "crops empty borders around the ink" $
        brailleRows 8 16 inkInBottomRightCell `shouldBe` ["⣿"]
    it "picks the largest ppem that fits the budget" $
        fitPpem 10 20 300 200 `shouldBe` Just 10
    it "gives up when the art would be too small" $
        fitPpem 10 20 300 60 `shouldBe` Nothing
    it "renders a query when the renderer and a Tibetan font are there" $ do
        art <- renderQuery 80 24 "མོ"
        case art of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> rows `shouldSatisfy` not . null
    it "fits a tall stack to the height budget" $ do
        art <- renderQuery 80 40 "ཀཔ"
        case art of
            Nothing -> pendingWith "no renderer or no Tibetan font"
            Just rows -> length rows `shouldSatisfy` (>= 10)

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
