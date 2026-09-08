module Test.Grammar.Rule.Constraint18 (tests) where

import Convert.Grammar.Parser
import Convert.Grammar.Rule.Constraint18 (pConstraint18)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 18"
    [ testCase "parses root ཧ above subroot ཕ" $
        parseRaws pConstraint18 "ཧྥ" @?= Right ["ཧ", "ྥ"]
    , testCase "parses root ཧ with vowel above subroot ཕ" $
        parseRaws pConstraint18 "ཧྥི" @?= Right ["ཧ", "ྥ", "ི"]
    , testCase "rejects wrong subroot above ཧ" $
        isLeft (parseRaws pConstraint18 "ཧྲ") @?= True
    , testCase "rejects root ཀ above subroot ཕ" $
        isLeft (parseRaws pConstraint18 "ཀྥ") @?= True
    ]

parseRaws :: Parser [Token] -> Text -> Either Text [Text]
parseRaws p input = fmap (map tokenRaw) $ parseEither p (tokenizeUnicode input)