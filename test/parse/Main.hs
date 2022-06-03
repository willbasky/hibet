module Main where

import Parse.Script
import Parse.Type
-- import Type (HibetError (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Control.Monad.Except (runExcept)
-- import Text.Megaparsec (parseTest)

main :: IO ()
main = hspec $ do
  tibetan

tibetan :: Spec
tibetan = do
  describe "Parse tibetan words" $ do
    it "མ་" $ do
      let ma = "མ་"
      let res = runExcept $ parseT tibetanScript "" ma
      res `shouldBe` Right ["མ"]
    it "མེ་" $ do
      let me = "མེ་"
      let res = runExcept $ parseT tibetanScript "" me
      res `shouldBe` Right ["མེ"]
    it "མ་མ་" $ do
      let mama = "མ་མ་"
      let res = runExcept $ parseT tibetanScript "" mama
      res `shouldBe` Right ["མ","མ"]
    it "མ་མི་ངྼ།" $ do
      let mamigr = "མ་མི་ངྼ།"
      let res = runExcept $ parseT tibetanScript "" mamigr
      res `shouldBe` Right ["མ","མི","ངྼ"]
