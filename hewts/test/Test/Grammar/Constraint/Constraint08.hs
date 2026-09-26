module Test.Grammar.Constraint.Constraint08 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser (Parser, parseEither)
import Convert.Grammar.Word (Position (..), TibetanWord)
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
        , testCase "marks each letter of the word" $
            parsePositions pConstraint08 "རྒོ" @?= Right [Superfix, Root, Vowel]
        ]

parseRaws :: Parser TibetanWord -> Text -> Either Text [Text]
parseRaws p input = fmap (map (tokenRaw . snd)) $ parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanWord -> Text -> Either Text [Position]
parsePositions p input = fmap (map fst) $ parseEither p (fst (tokenizeUnicode input))
