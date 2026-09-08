module Test.Grammar.Rule.Constraint11 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint11 (pConstraint11)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 11"
    [ testCase "ba-prefix + ra-superfix + roots1 without vowel" $
        parseRaws pConstraint11 "བརྐ" @?= Right ["བ", "ར", "ྐ"]
    , testCase "ba-prefix + ra-superfix + roots1 with vowel" $
        parseRaws pConstraint11 "བརྐུ" @?= Right ["བ", "ར", "ྐ", "ུ"]
    , testCase "ba-prefix + la-superfix + roots2 without vowel" $
        parseRaws pConstraint11 "བལྟ" @?= Right ["བ", "ལ", "ྟ"]
    , testCase "ba-prefix + la-superfix + roots2 with vowel" $
        parseRaws pConstraint11 "བལྟོ" @?= Right ["བ", "ལ", "ྟ", "ོ"]
    , testCase "ba-prefix + sa-superfix + roots3 without vowel" $
        parseRaws pConstraint11 "བསྐ" @?= Right ["བ", "ས", "ྐ"]
    , testCase "ba-prefix + sa-superfix + roots3 with vowel" $
        parseRaws pConstraint11 "བསྐུ" @?= Right ["བ", "ས", "ྐ", "ུ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
