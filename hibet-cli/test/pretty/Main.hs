module Main where

import Dictionary (Answer (..), Target (Target))
import Label (Title (Title))
import Pretty (paginate, viewTranslations)
import Shell (mkOutput, queryScale)

import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import qualified Data.Text as T
import Test.Hspec
    ( Spec
    , describe
    , hspec
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )

main :: IO ()
main = hspec $ do
    viewTranslationsSpec
    mkOutputSpec
    queryScaleSpec
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
mkOutputSpec = describe "mkOutput" $ do
    it "draws a short query at four times the cell size" $
        T.unpack
            (renderStrict (layoutPretty defaultLayoutOptions $ mkOutput 4 "མོ" [answer1]))
            `shouldContain` "\ESC]66;s=4;མོ\a\ESC[3B"
    it "leaves the query as plain text at scale one" $
        T.unpack
            (renderStrict (layoutPretty defaultLayoutOptions $ mkOutput 1 "མོ" [answer1]))
            `shouldNotContain` "\ESC]66;"

queryScaleSpec :: Spec
queryScaleSpec = describe "queryScale" $ do
    it "uses four times when there is room" $
        queryScale 80 "མོ" `shouldBe` 4
    it "falls back to plain when the query is wide" $
        queryScale 12 "སེམས་ཅན" `shouldBe` 1
    it "never goes below one" $
        queryScale 0 "མོ" `shouldBe` 1

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
