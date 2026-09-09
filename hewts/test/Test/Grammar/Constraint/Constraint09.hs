module Test.Grammar.Constraint.Constraint09 (tests) where

import Convert.Grammar
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 09"
    [ testCase "wa-subfix without vowel" $
        parseRaws pConstraint09 "ཀྭ" @?= Right ["ཀ", "ྭ"]
    , testCase "wa-subfix with vowel" $
        parseRaws pConstraint09 "ཀྭུ" @?= Right ["ཀ", "ྭ", "ུ"]
    , testCase "ya-subfix without vowel" $
        parseRaws pConstraint09 "ཀྱ" @?= Right ["ཀ", "ྱ"]
    , testCase "ya-subfix with vowel" $
        parseRaws pConstraint09 "ཀྱི" @?= Right ["ཀ", "ྱ", "ི"]
    , testCase "ra-subfix without vowel" $
        parseRaws pConstraint09 "ཀྲ" @?= Right ["ཀ", "ྲ"]
    , testCase "ra-subfix with vowel" $
        parseRaws pConstraint09 "ཀྲེ" @?= Right ["ཀ", "ྲ", "ེ"]
    , testCase "la-subfix without vowel" $
        parseRaws pConstraint09 "ཀླ" @?= Right ["ཀ", "ླ"]
    , testCase "la-subfix with vowel" $
        parseRaws pConstraint09 "ཀླེ" @?= Right ["ཀ", "ླ", "ེ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
