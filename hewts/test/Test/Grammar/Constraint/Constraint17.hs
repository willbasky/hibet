-- Ожидания: книга (Nyima Tashi), правило 17.

module Test.Grammar.Constraint.Constraint17 (tests) where

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
        "grammar rule 17"
        [ testCase "parses root ག above subfix ར" $
            parseRaws (pConstraint17Ra) "གྲྭ" @?= Right ["ག", "ྲ", "ྭ"]
        , testCase "parses root ད above subfix ར" $
            parseRaws (pConstraint17Ra) "དྲྭ" @?= Right ["ད", "ྲ", "ྭ"]
        , testCase "parses root ག with vowel above subfix ར" $
            parseRaws (pConstraint17Ra) "གྲྭི" @?= Right ["ག", "ྲ", "ྭ", "ི"]
        , testCase "rejects root ཀ above subfix ར" $
            isLeft (parseRaws (pConstraint17Ra) "ཀྲྭ") @?= True
        , testCase "rejects missing subfix ཝ" $
            isLeft (parseRaws (pConstraint17Ra) "གྲ") @?= True
        , testCase "parses root ཕ above subfix ཡ" $
            parseRaws (pConstraint17Ya) "ཕྱྭ" @?= Right ["ཕ", "ྱ", "ྭ"]
        , testCase "parses root ཕ with vowel above subfix ཡ" $
            parseRaws (pConstraint17Ya) "ཕྱྭི" @?= Right ["ཕ", "ྱ", "ྭ", "ི"]
        , testCase "rejects root ག above subfix ཡ" $
            isLeft (parseRaws (pConstraint17Ya) "གྱྭ") @?= True
        , testCase "rejects missing subfix ཝ" $
            isLeft (parseRaws (pConstraint17Ya) "ཕྱ") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint17Ra) "གྲྭ" @?= Right [RoleRoot]
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
