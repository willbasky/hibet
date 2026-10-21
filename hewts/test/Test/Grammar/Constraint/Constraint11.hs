-- Ожидания: книга (Nyima Tashi), правило 11.

module Test.Grammar.Constraint.Constraint11 (tests) where

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
        "grammar rule 11"
        [ testCase "ba-prefix + ra-superfix + roots1 without vowel" $
            parseRaws (pConstraint11) "བརྐ" @?= Right ["བ", "ར", "ྐ"]
        , testCase "ba-prefix + ra-superfix + roots1 with vowel" $
            parseRaws (pConstraint11) "བརྐུ" @?= Right ["བ", "ར", "ྐ", "ུ"]
        , testCase "ba-prefix + la-superfix + roots2 without vowel" $
            parseRaws (pConstraint11) "བལྟ" @?= Right ["བ", "ལ", "ྟ"]
        , testCase "ba-prefix + la-superfix + roots2 with vowel" $
            parseRaws (pConstraint11) "བལྟོ" @?= Right ["བ", "ལ", "ྟ", "ོ"]
        , testCase "ba-prefix + sa-superfix + roots3 without vowel" $
            parseRaws (pConstraint11) "བསྐ" @?= Right ["བ", "ས", "ྐ"]
        , testCase "ba-prefix + sa-superfix + roots3 with vowel" $
            parseRaws (pConstraint11) "བསྐུ" @?= Right ["བ", "ས", "ྐ", "ུ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint11) "བརྐུ" @?= Right [RolePrefix, RoleRoot]
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
