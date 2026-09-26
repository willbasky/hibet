module Test.Grammar.Constraint.Constraint17 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser (Parser, parseEither)
import Convert.Grammar.Word (Position (..), TibetanWord)
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
    , testCase "marks each letter of the word" $
        parsePositions pConstraint17Ra "གྲྭ" @?= Right [Root, Subfix, Subfix]
    ]

parseRaws :: Parser TibetanWord -> Text -> Either Text [Text]
parseRaws p input = fmap (map (tokenRaw . snd)) $ parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanWord -> Text -> Either Text [Position]
parsePositions p input = fmap (map fst) $ parseEither p (fst (tokenizeUnicode input))
