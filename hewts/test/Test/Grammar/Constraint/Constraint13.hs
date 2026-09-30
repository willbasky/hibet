module Test.Grammar.Constraint.Constraint13 (tests) where

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
        "grammar rule 13"
        [ testCase "ba-prefix + sa-superfix + roots + ya-subfix without vowel" $
            parseRaws (pConstraint13 Tibetan) "བསྐྱ" @?= Right ["བ", "ས", "ྐ", "ྱ"]
        , testCase "ba-prefix + sa-superfix + roots + ya-subfix with vowel" $
            parseRaws (pConstraint13 Tibetan) "བསྐྱུ" @?= Right ["བ", "ས", "ྐ", "ྱ", "ུ"]
        , testCase "ba-prefix + sa-superfix + roots + ra-subfix without vowel" $
            parseRaws (pConstraint13 Tibetan) "བསྐྲ" @?= Right ["བ", "ས", "ྐ", "ྲ"]
        , testCase "ba-prefix + sa-superfix + roots + ra-subfix with vowel" $
            parseRaws (pConstraint13 Tibetan) "བསྐྲོ" @?= Right ["བ", "ས", "ྐ", "ྲ", "ོ"]
        , testCase "ba-prefix + ra-superfix + roots + ya-subfix without vowel" $
            parseRaws (pConstraint13 Tibetan) "བརྐྱ" @?= Right ["བ", "ར", "ྐ", "ྱ"]
        , testCase "ba-prefix + ra-superfix + roots + ya-subfix with vowel" $
            parseRaws (pConstraint13 Tibetan) "བརྐྱུ" @?= Right ["བ", "ར", "ྐ", "ྱ", "ུ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint13 Tibetan) "བསྐྱུ"
                @?= Right [Prefix, Superfix, Root, Subfix, Vowel]
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
