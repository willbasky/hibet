-- Ожидания: книга (Nyima Tashi), правило 10.

module Test.Grammar.Constraint.Constraint10 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
    , parseEither
    , runSpell
    , takeStacks
    )
import Convert.Grammar.Syllable (Role (..), Stack (..), TibetanSyllable)
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
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint10) "རྐྱི" @?= Right [RoleRoot]
        ]

parseRaws :: SpellParser TibetanSyllable -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap tokenRaw) $
        parseEither (runSpell p) (fst (tokenizeUnicode input))

-- | The roles of the stacks the rule built, read from the parser state: the
-- role is a field of the stack now, not a mark on the letter (REFACTOR-AST
-- step 4e). The letters are checked by 'parseRaws', and the slots inside a
-- stack by @test/golden/stacks.golden@.
parseRoles :: SpellParser TibetanSyllable -> Text -> Either Text [Role]
parseRoles p input =
    fmap (map stRole . toList) $
        parseEither (runSpell (p *> takeStacks)) (fst (tokenizeUnicode input))
