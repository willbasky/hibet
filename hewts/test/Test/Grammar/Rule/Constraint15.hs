module Test.Grammar.Rule.Constraint15 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint15 (pConstraint15)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 15"
    [ testCase "parses suffix ག" $
        parseRaws pConstraint15 "ག" @?= Right ["ག"]
    , testCase "parses suffix ང" $
        parseRaws pConstraint15 "ང" @?= Right ["ང"]
    , testCase "parses suffix ད" $
        parseRaws pConstraint15 "ད" @?= Right ["ད"]
    , testCase "parses suffix ན" $
        parseRaws pConstraint15 "ན" @?= Right ["ན"]
    , testCase "parses suffix བ" $
        parseRaws pConstraint15 "བ" @?= Right ["བ"]
    , testCase "parses suffix མ" $
        parseRaws pConstraint15 "མ" @?= Right ["མ"]
    , testCase "parses suffix འ" $
        parseRaws pConstraint15 "འ" @?= Right ["འ"]
    , testCase "parses suffix ར" $
        parseRaws pConstraint15 "ར" @?= Right ["ར"]
    , testCase "parses suffix ལ" $
        parseRaws pConstraint15 "ལ" @?= Right ["ལ"]
    , testCase "parses suffix ས" $
        parseRaws pConstraint15 "ས" @?= Right ["ས"]
    , testCase "rejects non-suffix consonant ཀ" $
        isLeft (parseRaws pConstraint15 "ཀ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)