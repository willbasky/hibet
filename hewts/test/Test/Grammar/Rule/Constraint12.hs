module Test.Grammar.Rule.Constraint12 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint12 (pConstraint12)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 12"
    [ testCase "da-prefix + roots1 + ya-subfix without vowel" $
        parseRaws pConstraint12 "དཀྱ" @?= Right ["ད", "ཀ", "ྱ"]
    , testCase "da-prefix + roots1 + ya-subfix with vowel" $
        parseRaws pConstraint12 "དཀྱུ" @?= Right ["ད", "ཀ", "ྱ", "ུ"]
    , testCase "da-prefix + roots2 + ra-subfix without vowel" $
        parseRaws pConstraint12 "དཀྲ" @?= Right ["ད", "ཀ", "ྲ"]
    , testCase "da-prefix + roots2 + ra-subfix with vowel" $
        parseRaws pConstraint12 "དཀྲོ" @?= Right ["ད", "ཀ", "ྲ", "ོ"]
    , testCase "ba-prefix + roots3 + ya-subfix without vowel" $
        parseRaws pConstraint12 "བཀྱ" @?= Right ["བ", "ཀ", "ྱ"]
    , testCase "ba-prefix + roots3 + ya-subfix with vowel" $
        parseRaws pConstraint12 "བཀྱུ" @?= Right ["བ", "ཀ", "ྱ", "ུ"]
    , testCase "ba-prefix + roots4 + ra-subfix without vowel" $
        parseRaws pConstraint12 "བཀྲ" @?= Right ["བ", "ཀ", "ྲ"]
    , testCase "ba-prefix + roots4 + ra-subfix with vowel" $
        parseRaws pConstraint12 "བཀྲོ" @?= Right ["བ", "ཀ", "ྲ", "ོ"]
    , testCase "ba-prefix + roots5 + la-subfix without vowel" $
        parseRaws pConstraint12 "བཀླ" @?= Right ["བ", "ཀ", "ླ"]
    , testCase "ba-prefix + roots5 + la-subfix with vowel" $
        parseRaws pConstraint12 "བཀློ" @?= Right ["བ", "ཀ", "ླ", "ོ"]
    , testCase "ma-prefix + roots6 + ya-subfix without vowel" $
        parseRaws pConstraint12 "མཁྱ" @?= Right ["མ", "ཁ", "ྱ"]
    , testCase "ma-prefix + roots6 + ra-subfix without vowel" $
        parseRaws pConstraint12 "མཁྲ" @?= Right ["མ", "ཁ", "ྲ"]
    , testCase "a-prefix + roots7 + ya-subfix without vowel" $
        parseRaws pConstraint12 "འཁྱ" @?= Right ["འ", "ཁ", "ྱ"]
    , testCase "a-prefix + roots7 + ya-subfix with vowel" $
        parseRaws pConstraint12 "འཁྱུ" @?= Right ["འ", "ཁ", "ྱ", "ུ"]
    , testCase "a-prefix + roots8 + ra-subfix without vowel" $
        parseRaws pConstraint12 "འཁྲ" @?= Right ["འ", "ཁ", "ྲ"]
    , testCase "a-prefix + roots8 + ra-subfix with vowel" $
        parseRaws pConstraint12 "འཁྲོ" @?= Right ["འ", "ཁ", "ྲ", "ོ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
