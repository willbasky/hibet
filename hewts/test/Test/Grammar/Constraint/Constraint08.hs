module Test.Grammar.Constraint.Constraint08 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
    , Spelling (..)
    , parseEither
    , runSpell
    )
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar rule 08"
        [ testCase "ra-superfix form without vowel" $
            parseRaws (pConstraint08 Tibetan) "རྒ" @?= Right ["ར", "ྒ"]
        , testCase "ra-superfix form with vowel" $
            parseRaws (pConstraint08 Tibetan) "རྒོ" @?= Right ["ར", "ྒ", "ོ"]
        , testCase "la-superfix form without vowel" $
            parseRaws (pConstraint08 Tibetan) "ལྤ" @?= Right ["ལ", "ྤ"]
        , testCase "la-superfix form with vowel" $
            parseRaws (pConstraint08 Tibetan) "ལྤོ" @?= Right ["ལ", "ྤ", "ོ"]
        , testCase "sa-superfix form without vowel" $
            parseRaws (pConstraint08 Tibetan) "སྨ" @?= Right ["ས", "ྨ"]
        , testCase "sa-superfix form with vowel" $
            parseRaws (pConstraint08 Tibetan) "སྨོ" @?= Right ["ས", "ྨ", "ོ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint08 Tibetan) "རྒོ" @?= Right [Superfix, Root, Vowel]
        ]

parseRaws :: SpellParser TibetanSyllable -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither (runSpell p) (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: SpellParser TibetanSyllable -> Text -> Either Text [Position]
parsePositions p input =
    fmap (toList . fmap fst) $
        parseEither (runSpell p) (fst (tokenizeUnicode input))
