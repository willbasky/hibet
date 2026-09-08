module Test.Grammar.Rule.Constraint19 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint19 (pConstraint19)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 19"
    [ testCase "parses root ཧ subroot ཕ with suffix ག" $
        parseRaws pConstraint19 "ཧྥག" @?= Right ["ཧ", "ྥ", "ག"]
    , testCase "parses struct with vowel and suffix ས" $
        parseRaws pConstraint19 "ཧྥིས" @?= Right ["ཧ", "ྥ", "ི", "ས"]
    , testCase "rejects suffix ཀ not in grammar 15" $
        isLeft (parseRaws pConstraint19 "ཧྥཀ") @?= True
    , testCase "rejects missing suffix" $
        isLeft (parseRaws pConstraint19 "ཧྥ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)