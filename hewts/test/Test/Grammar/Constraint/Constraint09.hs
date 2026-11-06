-- Ожидания: книга (Nyima Tashi), правило 09.

module Test.Grammar.Constraint.Constraint09 (tests) where

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
        "grammar rule 09"
        [ testCase "wa-subfix without vowel" $
            parseRaws (pConstraint09) "ཀྭ" @?= Right ["ཀ", "ྭ"]
        , testCase "wa-subfix with vowel" $
            parseRaws (pConstraint09) "ཀྭུ" @?= Right ["ཀ", "ྭ", "ུ"]
        , testCase "ya-subfix without vowel" $
            parseRaws (pConstraint09) "ཀྱ" @?= Right ["ཀ", "ྱ"]
        , testCase "ya-subfix with vowel" $
            parseRaws (pConstraint09) "ཀྱི" @?= Right ["ཀ", "ྱ", "ི"]
        , testCase "ra-subfix without vowel" $
            parseRaws (pConstraint09) "ཀྲ" @?= Right ["ཀ", "ྲ"]
        , testCase "ra-subfix with vowel" $
            parseRaws (pConstraint09) "ཀྲེ" @?= Right ["ཀ", "ྲ", "ེ"]
        , testCase "la-subfix without vowel" $
            parseRaws (pConstraint09) "ཀླ" @?= Right ["ཀ", "ླ"]
        , testCase "la-subfix with vowel" $
            parseRaws (pConstraint09) "ཀླེ" @?= Right ["ཀ", "ླ", "ེ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint09) "ཀྭུ" @?= Right [RoleRoot]
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
