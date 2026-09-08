module Test.Grammar.Rule.Constraint14 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint14 (pConstraint14)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 14"
    [ testCase "ga-prefix + roots1 without vowel" $
        parseRaws pConstraint14 "གཅ" @?= Right ["ག", "ཅ"]
    , testCase "ga-prefix + roots1 with vowel" $
        parseRaws pConstraint14 "གཅུ" @?= Right ["ག", "ཅ", "ུ"]
    , testCase "da-prefix + roots2 without vowel" $
        parseRaws pConstraint14 "དཀ" @?= Right ["ད", "ཀ"]
    , testCase "da-prefix + roots2 with vowel" $
        parseRaws pConstraint14 "དཀུ" @?= Right ["ད", "ཀ", "ུ"]
    , testCase "ba-prefix + roots3 without vowel" $
        parseRaws pConstraint14 "བཀ" @?= Right ["བ", "ཀ"]
    , testCase "ba-prefix + roots3 with vowel" $
        parseRaws pConstraint14 "བཀུ" @?= Right ["བ", "ཀ", "ུ"]
    , testCase "ma-prefix + roots4 without vowel" $
        parseRaws pConstraint14 "མཁ" @?= Right ["མ", "ཁ"]
    , testCase "ma-prefix + roots4 with vowel" $
        parseRaws pConstraint14 "མཁུ" @?= Right ["མ", "ཁ", "ུ"]
    , testCase "a-prefix + roots5 without vowel" $
        parseRaws pConstraint14 "འཁ" @?= Right ["འ", "ཁ"]
    , testCase "a-prefix + roots5 with vowel" $
        parseRaws pConstraint14 "འཁུ" @?= Right ["འ", "ཁ", "ུ"]
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)
