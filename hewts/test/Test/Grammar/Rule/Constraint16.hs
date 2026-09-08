module Test.Grammar.Rule.Constraint16 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint16 (pConstraint16Da, pConstraint16Sa)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 16"
    [ testCase "parses suffix ན before postfix ད" $
        parseRaws pConstraint16Da "ན" @?= Right ["ན"]
    , testCase "parses suffix ར before postfix ད" $
        parseRaws pConstraint16Da "ར" @?= Right ["ར"]
    , testCase "parses suffix ལ before postfix ད" $
        parseRaws pConstraint16Da "ལ" @?= Right ["ལ"]
    , testCase "rejects ག for postfix ད rule" $
        isLeft (parseRaws pConstraint16Da "ག") @?= True
    , testCase "parses suffix ག before postfix ས" $
        parseRaws pConstraint16Sa "ག" @?= Right ["ག"]
    , testCase "parses suffix ང before postfix ས" $
        parseRaws pConstraint16Sa "ང" @?= Right ["ང"]
    , testCase "parses suffix བ before postfix ས" $
        parseRaws pConstraint16Sa "བ" @?= Right ["བ"]
    , testCase "parses suffix མ before postfix ས" $
        parseRaws pConstraint16Sa "མ" @?= Right ["མ"]
    , testCase "rejects ན for postfix ས rule" $
        isLeft (parseRaws pConstraint16Sa "ན") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)