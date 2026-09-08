module Test.Grammar.Rule.Constraint13 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint13 (pConstraint13)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 13"
    [ testCase "ba-prefix + sa-superfix + roots + ya-subfix without vowel" $
        parseRaws pConstraint13 "བསྐྱ" @?= Right ["བ", "ས", "ྐ", "ྱ"]
    , testCase "ba-prefix + sa-superfix + roots + ya-subfix with vowel" $
        parseRaws pConstraint13 "བསྐྱུ" @?= Right ["བ", "ས", "ྐ", "ྱ", "ུ"]
    , testCase "ba-prefix + sa-superfix + roots + ra-subfix without vowel" $
        parseRaws pConstraint13 "བསྐྲ" @?= Right ["བ", "ས", "ྐ", "ྲ"]
    , testCase "ba-prefix + sa-superfix + roots + ra-subfix with vowel" $
        parseRaws pConstraint13 "བསྐྲོ" @?= Right ["བ", "ས", "ྐ", "ྲ", "ོ"]
    , testCase "ba-prefix + ra-superfix + roots + ya-subfix without vowel" $
        parseRaws pConstraint13 "བརྐྱ" @?= Right ["བ", "ར", "ྐ", "ྱ"]
    , testCase "ba-prefix + ra-superfix + roots + ya-subfix with vowel" $
        parseRaws pConstraint13 "བརྐྱུ" @?= Right ["བ", "ར", "ྐ", "ྱ", "ུ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
