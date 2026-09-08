module Test.Grammar.Rule.Constraint20 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint20 (pConstraint20)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 20"
    [ testCase "parses root འ" $
        parseRaws pConstraint20 "འ" @?= Right ["འ"]
    , testCase "parses root འ with vowel" $
        parseRaws pConstraint20 "འི" @?= Right ["འ", "ི"]
    , testCase "parses root འ with subroot ང" $
        parseRaws pConstraint20 "འྔ" @?= Right ["འ", "ྔ"]
    , testCase "parses root འ with subroot མ" $
        parseRaws pConstraint20 "འྨ" @?= Right ["འ", "ྨ"]
    , testCase "rejects root ཀ" $
        isLeft (parseRaws pConstraint20 "ཀ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)