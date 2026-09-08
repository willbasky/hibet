module Test.Grammar.Rule.Constraint10 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint10 (pConstraint10)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 10"
    [ testCase "ra-superfix + roots1 + ya-subfix without vowel" $
        parseRaws pConstraint10 "རྐྱ" @?= Right ["ར", "ྐ", "ྱ"]
    , testCase "ra-superfix + roots1 + ya-subfix with vowel" $
        parseRaws pConstraint10 "རྐྱི" @?= Right ["ར", "ྐ", "ྱ", "ི"]
    , testCase "sa-superfix + roots2 + ya-subfix without vowel" $
        parseRaws pConstraint10 "སྐྱ" @?= Right ["ས", "ྐ", "ྱ"]
    , testCase "sa-superfix + roots2 + ra-subfix without vowel" $
        parseRaws pConstraint10 "སྐྲ" @?= Right ["ས", "ྐ", "ྲ"]
    , testCase "sa-superfix + root-na + ra-subfix without vowel" $
        parseRaws pConstraint10 "སྣྲ" @?= Right ["ས", "ྣ", "ྲ"]
    , testCase "ra-superfix + root-tsa + wa-subfix without vowel" $
        parseRaws pConstraint10 "རྩྭ" @?= Right ["ར", "ྩ", "ྭ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
