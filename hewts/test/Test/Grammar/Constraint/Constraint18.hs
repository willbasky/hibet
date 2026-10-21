-- Ожидания: книга (Nyima Tashi), правило 18.

module Test.Grammar.Constraint.Constraint18 (tests) where

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
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar rule 18"
        [ testCase "parses root ཧ above subroot ཕ" $
            parseRaws (pConstraint18) "ཧྥ" @?= Right ["ཧ", "ྥ"]
        , testCase "parses root ཧ with vowel above subroot ཕ" $
            parseRaws (pConstraint18) "ཧྥི" @?= Right ["ཧ", "ྥ", "ི"]
        , testCase "rejects wrong subroot above ཧ" $
            isLeft (parseRaws (pConstraint18) "ཧྲ") @?= True
        , testCase "rejects root ཀ above subroot ཕ" $
            isLeft (parseRaws (pConstraint18) "ཀྥ") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint18) "ཧྥི" @?= Right [RoleRoot]
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
