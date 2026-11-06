-- Ожидания: книга (Nyima Tashi), правило 08.

module Test.Grammar.Constraint.Constraint08 (tests) where

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
        "grammar rule 08"
        [ testCase "ra-superfix form without vowel" $
            parseRaws (pConstraint08) "རྒ" @?= Right ["ར", "ྒ"]
        , testCase "ra-superfix form with vowel" $
            parseRaws (pConstraint08) "རྒོ" @?= Right ["ར", "ྒ", "ོ"]
        , testCase "la-superfix form without vowel" $
            parseRaws (pConstraint08) "ལྤ" @?= Right ["ལ", "ྤ"]
        , testCase "la-superfix form with vowel" $
            parseRaws (pConstraint08) "ལྤོ" @?= Right ["ལ", "ྤ", "ོ"]
        , testCase "sa-superfix form without vowel" $
            parseRaws (pConstraint08) "སྨ" @?= Right ["ས", "ྨ"]
        , testCase "sa-superfix form with vowel" $
            parseRaws (pConstraint08) "སྨོ" @?= Right ["ས", "ྨ", "ོ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint08) "རྒོ" @?= Right [RoleRoot]
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
