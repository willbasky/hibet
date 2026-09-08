module Test.Grammar.Rule.Constraint08 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint08 (pConstraint08)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
    testGroup
        "grammar rule 08"
        [ testCase "ra-superfix form without vowel" $
            parseRaws pConstraint08 "རྒ" @?= Right ["ར", "ྒ"]
        , testCase "ra-superfix form with vowel" $
            parseRaws pConstraint08 "རྒོ" @?= Right ["ར", "ྒ", "ོ"]
        , testCase "la-superfix form without vowel" $
            parseRaws pConstraint08 "ལྤ" @?= Right ["ལ", "ྤ"]
        , testCase "la-superfix form with vowel" $
            parseRaws pConstraint08 "ལྤོ" @?= Right ["ལ", "ྤ", "ོ"]
        , testCase "sa-superfix form without vowel" $
            parseRaws pConstraint08 "སྨ" @?= Right ["ས", "ྨ"]
        , testCase "sa-superfix form with vowel" $
            parseRaws pConstraint08 "སྨོ" @?= Right ["ས", "ྨ", "ོ"]
        ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
