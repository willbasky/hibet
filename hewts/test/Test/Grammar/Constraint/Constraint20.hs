-- Ожидания: книга (Nyima Tashi), правило 20.

module Test.Grammar.Constraint.Constraint20 (tests) where

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
        "grammar rule 20"
        [ testCase "parses root འ" $
            parseRaws (pConstraint20) "འ" @?= Right ["འ"]
        , testCase "parses root འ with vowel" $
            parseRaws (pConstraint20) "འི" @?= Right ["འ", "ི"]
        , testCase "parses root འ with subroot ང" $
            parseRaws (pConstraint20) "འྔ" @?= Right ["འ", "ྔ"]
        , testCase "parses root འ with subroot མ" $
            parseRaws (pConstraint20) "འྨ" @?= Right ["འ", "ྨ"]
        , testCase "rejects root ཀ" $
            isLeft (parseRaws (pConstraint20) "ཀ") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint20) "འོ" @?= Right [RoleRoot]
        , testCase "a bare a-chung is the word's last suffix" $
            parseRoles (pConstraint20) "འ" @?= Right [RoleSuffix]
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
