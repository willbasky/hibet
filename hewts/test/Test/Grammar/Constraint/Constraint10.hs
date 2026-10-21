-- Ожидания: книга (Nyima Tashi), правило 10.

module Test.Grammar.Constraint.Constraint10 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
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
        "grammar rule 10"
        [ testCase "ra-superfix + roots1 + ya-subfix without vowel" $
            parseRaws (pConstraint10) "རྐྱ" @?= Right ["ར", "ྐ", "ྱ"]
        , testCase "ra-superfix + roots1 + ya-subfix with vowel" $
            parseRaws (pConstraint10) "རྐྱི" @?= Right ["ར", "ྐ", "ྱ", "ི"]
        , testCase "sa-superfix + roots2 + ya-subfix without vowel" $
            parseRaws (pConstraint10) "སྐྱ" @?= Right ["ས", "ྐ", "ྱ"]
        , testCase "sa-superfix + roots2 + ra-subfix without vowel" $
            parseRaws (pConstraint10) "སྐྲ" @?= Right ["ས", "ྐ", "ྲ"]
        , testCase "sa-superfix + root-na + ra-subfix without vowel" $
            parseRaws (pConstraint10) "སྣྲ" @?= Right ["ས", "ྣ", "ྲ"]
        , testCase "ra-superfix + root-tsa + wa-subfix without vowel" $
            parseRaws (pConstraint10) "རྩྭ" @?= Right ["ར", "ྩ", "ྭ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint10) "རྐྱི"
                @?= Right [Superfix, Root, Subfix, Vowel]
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
