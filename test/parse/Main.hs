module Main where

import Parse.TibetanWord ( tibetanWord )
import Parse.WylieWord ( wylieWord )
import Parse.Type ( parseEither )
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Data.Either ( isLeft )

-- import Text.Megaparsec (parseTest)
-- import qualified Text.Megaparsec.Char as MC


main :: IO ()
main = hspec $ do
  tibetan
  wylie

tibetan :: Spec
tibetan = do
  describe "Parse tibetan words" $ do
    it "empty query" $ do
      let ma = ""
      let res = parseEither tibetanWord ma
      isLeft res `shouldBe` True
    it "space query" $ do
      let ma = " "
      let res = parseEither tibetanWord ma
      isLeft res `shouldBe` True
    it "འང་དེ" $ do
      let ma = "འང་དེ"
      let res = parseEither tibetanWord ma
      res `shouldBe` Right ["འང","དེ"]
    it "མ་" $ do
      let ma = "མ་"
      let res = parseEither tibetanWord ma
      res `shouldBe` Right ["མ"]
    it "མ with end space (without dot)" $ do
      let ma = "མ "
      let res = parseEither tibetanWord ma
      res `shouldBe` Right ["མ"]
    it "མ་ with end space" $ do
      let ma = "མ་ "
      let res = parseEither tibetanWord ma
      res `shouldBe` Right ["མ"]
    it "མ་ with start space" $ do
      let ma = " མ་"
      let res = parseEither tibetanWord ma
      res `shouldBe` Right ["མ"]
    it "མེ་" $ do
      let me = "མེ་"
      let res = parseEither tibetanWord me
      res `shouldBe` Right ["མེ"]
    it "མེ་ with end space" $ do
      let me = "མེ་ "
      let res = parseEither tibetanWord me
      res `shouldBe` Right ["མེ"]
    it "མེ་ with start space" $ do
      let me = " མེ་"
      let res = parseEither tibetanWord me
      res `shouldBe` Right ["མེ"]
    it "མ་མ་" $ do
      let mama = "མ་མ་"
      let res = parseEither tibetanWord mama
      res `shouldBe` Right ["མ","མ"]
    it "རེ་བ་མེད་པ།" $ do
      let mamigr = "རེ་བ་མེད་པ།"
      let res = parseEither tibetanWord mamigr
      res `shouldBe` Right ["རེ", "བ", "མེད", "པ"]
    it "རེ་བ་མེད་པ། with end space" $ do
      let mamigr = "རེ་བ་མེད་པ། "
      let res = parseEither tibetanWord mamigr
      res `shouldBe` Right ["རེ", "བ", "མེད", "པ"]
    it "རེ་བ་མེད་པ། with start space" $ do
      let mamigr = " རེ་བ་མེད་པ།"
      let res = parseEither tibetanWord mamigr
      res `shouldBe` Right ["རེ", "བ", "མེད", "པ"]
    it "རེ་ བ་ མེད་ པ། with space between syllables" $ do
      let mamigr = " རེ་ བ་ མེད་ པ། "
      let res = parseEither tibetanWord mamigr
      res `shouldBe` Right ["རེ", "བ", "མེད", "པ"]
    it "རེ བ མེད པ with space and without dots" $ do
      let mamigr = " རེ བ མེད པ "
      let res = parseEither tibetanWord mamigr
      res `shouldBe` Right ["རེ", "བ", "མེད", "པ"]

    it "ma rig pa - parse wylie" $ do
      let marigpa = "ma rig pa"
      let res = parseEither tibetanWord marigpa
      res `shouldBe` Right ["ma","rig","pa"]

wylie :: Spec
wylie = do
  describe "Parse wylie words" $ do
    it "'ang de" $ do
      let angde = "'ang de"
      let res = parseEither wylieWord angde
      res `shouldBe` Right ["'ang","de"]
    it "empty query" $ do
      let ma = ""
      let res = parseEither wylieWord ma
      isLeft res `shouldBe` True
    it "space query" $ do
      let ma = " "
      let res = parseEither wylieWord ma
      isLeft res `shouldBe` True
    it "ma rig pa" $ do
      let marigpa = "ma rig pa"
      let res = parseEither wylieWord marigpa
      res `shouldBe` Right ["ma","rig","pa"]
    it "ma rig pa with start space" $ do
      let marigpa = " ma rig pa"
      let res = parseEither wylieWord marigpa
      res `shouldBe` Right ["ma","rig","pa"]
    it "ma rig pa with end space" $ do
      let marigpa = "ma rig pa "
      let res = parseEither wylieWord marigpa
      res `shouldBe` Right ["ma","rig","pa"]
    it "ma rig pa with space around" $ do
      let marigpa = "ma rig pa "
      let res = parseEither wylieWord marigpa
      res `shouldBe` Right ["ma","rig","pa"]

    it "རེ་བ་མེད་པ། - parse tibetan" $ do
      let mamigr = "རེ་བ་མེད་པ།"
      let res = parseEither wylieWord mamigr
      isLeft res `shouldBe` True
