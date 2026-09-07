module Test.Grammar.Rule.Constraint01 (tests) where

import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Rule.Constraint01 (pConstraint01, pConstraint01Sanskrit, pConstraint01WithLong)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
    testGroup
        "grammar rule 01"
        [ testCase "pConstraint01 parses root only" $
            parseRaws pConstraint01 "ས" @?= Right ["ས"]
        , testCase "pConstraint01 parses root+vowel" $
            parseRaws pConstraint01 "སུ" @?= Right ["ས", "ུ"]
        , testCase "pConstraint01WithLong parses root+regular vowel" $
            parseRaws pConstraint01WithLong "དུ" @?= Right ["ད", "ུ"]
        , testCase "pConstraint01WithLong parses root+long A" $
            parseRaws pConstraint01WithLong "སཱ" @?= Right ["ས", "ཱ"]
        , testCase "pConstraint01Sanskrit parses sanskrit root only" $
            parseRaws pConstraint01Sanskrit "ཌ" @?= Right ["ཌ"]
        , testCase "pConstraint01Sanskrit parses sanskrit root+vowel" $
            parseRaws pConstraint01Sanskrit "ཌོ" @?= Right ["ཌ", "ོ"]
        ]

parseRaws :: GP.Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ GP.parseEither p (tokenizeUnicode input)
