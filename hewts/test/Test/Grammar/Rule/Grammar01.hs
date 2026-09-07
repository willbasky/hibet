module Test.Grammar.Rule.Grammar01 (tests) where

import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Rule.Grammar01 (pGrammar1, pGrammar1Sanskrit, pGrammar1WithLong)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
    testGroup
        "grammar rule 01"
        [ testCase "pGrammar1 parses root only" $
            parseRaws pGrammar1 "ས" @?= Right ["ས"]
        , testCase "pGrammar1 parses root+vowel" $
            parseRaws pGrammar1 "སུ" @?= Right ["ས", "ུ"]
        , testCase "pGrammar1WithLong parses root+regular vowel" $
            parseRaws pGrammar1WithLong "དུ" @?= Right ["ད", "ུ"]
        , testCase "pGrammar1WithLong parses root+long A" $
            parseRaws pGrammar1WithLong "སཱ" @?= Right ["ས", "ཱ"]
        , testCase "pGrammar1Sanskrit parses sanskrit root only" $
            parseRaws pGrammar1Sanskrit "ཌ" @?= Right ["ཌ"]
        , testCase "pGrammar1Sanskrit parses sanskrit root+vowel" $
            parseRaws pGrammar1Sanskrit "ཌོ" @?= Right ["ཌ", "ོ"]
        ]

parseRaws :: GP.Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ GP.parseEither p (tokenizeUnicode input)
