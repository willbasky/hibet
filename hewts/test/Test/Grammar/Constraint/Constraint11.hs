module Test.Grammar.Constraint.Constraint11 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser (Parser, Spelling (..), parseEither)
import Convert.Grammar.Word (Position (..), TibetanWord)
import Data.Foldable (toList)
import Convert.Token (Token, tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "grammar rule 11"
    [ testCase "ba-prefix + ra-superfix + roots1 without vowel" $
        parseRaws (pConstraint11 Tibetan) "བརྐ" @?= Right ["བ", "ར", "ྐ"]
    , testCase "ba-prefix + ra-superfix + roots1 with vowel" $
        parseRaws (pConstraint11 Tibetan) "བརྐུ" @?= Right ["བ", "ར", "ྐ", "ུ"]
    , testCase "ba-prefix + la-superfix + roots2 without vowel" $
        parseRaws (pConstraint11 Tibetan) "བལྟ" @?= Right ["བ", "ལ", "ྟ"]
    , testCase "ba-prefix + la-superfix + roots2 with vowel" $
        parseRaws (pConstraint11 Tibetan) "བལྟོ" @?= Right ["བ", "ལ", "ྟ", "ོ"]
    , testCase "ba-prefix + sa-superfix + roots3 without vowel" $
        parseRaws (pConstraint11 Tibetan) "བསྐ" @?= Right ["བ", "ས", "ྐ"]
    , testCase "ba-prefix + sa-superfix + roots3 with vowel" $
        parseRaws (pConstraint11 Tibetan) "བསྐུ" @?= Right ["བ", "ས", "ྐ", "ུ"]
    , testCase "marks each letter of the word" $
        parsePositions (pConstraint11 Tibetan) "བརྐུ" @?= Right [Prefix, Superfix, Root, Vowel]
    ]

parseRaws :: Parser TibetanWord -> Text -> Either Text [Text]
parseRaws p input = fmap (toList . fmap (tokenRaw . snd)) $ parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanWord -> Text -> Either Text [Position]
parsePositions p input = fmap (toList . fmap fst) $ parseEither p (fst (tokenizeUnicode input))
