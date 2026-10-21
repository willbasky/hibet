-- Ожидания: книга (Nyima Tashi), правило 13.

module Test.Grammar.Constraint.Constraint13 (tests) where

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
        "grammar rule 13"
        [ testCase "ba-prefix + sa-superfix + roots + ya-subfix without vowel" $
            parseRaws (pConstraint13) "བསྐྱ" @?= Right ["བ", "ས", "ྐ", "ྱ"]
        , testCase "ba-prefix + sa-superfix + roots + ya-subfix with vowel" $
            parseRaws (pConstraint13) "བསྐྱུ" @?= Right ["བ", "ས", "ྐ", "ྱ", "ུ"]
        , testCase "ba-prefix + sa-superfix + roots + ra-subfix without vowel" $
            parseRaws (pConstraint13) "བསྐྲ" @?= Right ["བ", "ས", "ྐ", "ྲ"]
        , testCase "ba-prefix + sa-superfix + roots + ra-subfix with vowel" $
            parseRaws (pConstraint13) "བསྐྲོ" @?= Right ["བ", "ས", "ྐ", "ྲ", "ོ"]
        , testCase "ba-prefix + ra-superfix + roots + ya-subfix without vowel" $
            parseRaws (pConstraint13) "བརྐྱ" @?= Right ["བ", "ར", "ྐ", "ྱ"]
        , testCase "ba-prefix + ra-superfix + roots + ya-subfix with vowel" $
            parseRaws (pConstraint13) "བརྐྱུ" @?= Right ["བ", "ར", "ྐ", "ྱ", "ུ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint13) "བསྐྱུ" @?= Right [RolePrefix, RoleRoot]
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
