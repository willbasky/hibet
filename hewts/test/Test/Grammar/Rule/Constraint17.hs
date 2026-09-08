module Test.Grammar.Rule.Constraint17 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint17 (pConstraint17Ra, pConstraint17Ya)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 17"
    [ testCase "parses root ག above subfix ར" $
        parseRaws pConstraint17Ra "གྲྭ" @?= Right ["ག", "ྲ", "ྭ"]
    , testCase "parses root ད above subfix ར" $
        parseRaws pConstraint17Ra "དྲྭ" @?= Right ["ད", "ྲ", "ྭ"]
    , testCase "parses root ག with vowel above subfix ར" $
        parseRaws pConstraint17Ra "གྲྭི" @?= Right ["ག", "ྲ", "ྭ", "ི"]
    , testCase "rejects root ཀ above subfix ར" $
        isLeft (parseRaws pConstraint17Ra "ཀྲྭ") @?= True
    , testCase "rejects missing subfix ཝ" $
        isLeft (parseRaws pConstraint17Ra "གྲ") @?= True
    , testCase "parses root ཕ above subfix ཡ" $
        parseRaws pConstraint17Ya "ཕྱྭ" @?= Right ["ཕ", "ྱ", "ྭ"]
    , testCase "parses root ཕ with vowel above subfix ཡ" $
        parseRaws pConstraint17Ya "ཕྱྭི" @?= Right ["ཕ", "ྱ", "ྭ", "ི"]
    , testCase "rejects root ག above subfix ཡ" $
        isLeft (parseRaws pConstraint17Ya "གྱྭ") @?= True
    , testCase "rejects missing subfix ཝ" $
        isLeft (parseRaws pConstraint17Ya "ཕྱ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)