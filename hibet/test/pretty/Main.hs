module Main where

import Dictionary (Answer (..), Target (Target))
import Label (Title (Title))
import Pretty (viewTranslations)

import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  viewTranslationsSpec

viewTranslationsSpec :: Spec
viewTranslationsSpec =
  describe "Elem Text" $
    it "Success with Test 1" $ renderStrict (layoutPretty defaultLayoutOptions $ viewTranslations [answer1, answer2]) `shouldBe` "1. Dict1\n  target\n \n2. Dict2\n  target2"


answer1 :: Answer
answer1 = Answer
  { targets    = [Target "target"]
  , dictNumber = 1
  , dictTitle  = Title "Dict1"
  }

answer2 :: Answer
answer2 = Answer
  { targets    = [Target "target2"]
  , dictNumber = 2
  , dictTitle  = Title "Dict2"
  }
