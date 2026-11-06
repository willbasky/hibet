-- Ожидания: книга (Nyima Tashi), правило 12.

module Test.Grammar.Constraint.Constraint12 (tests) where

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
        "grammar rule 12"
        [ testCase "da-prefix + roots1 + ya-subfix without vowel" $
            parseRaws (pConstraint12) "དཀྱ" @?= Right ["ད", "ཀ", "ྱ"]
        , testCase "da-prefix + roots1 + ya-subfix with vowel" $
            parseRaws (pConstraint12) "དཀྱུ" @?= Right ["ད", "ཀ", "ྱ", "ུ"]
        , testCase "da-prefix + roots2 + ra-subfix without vowel" $
            parseRaws (pConstraint12) "དཀྲ" @?= Right ["ད", "ཀ", "ྲ"]
        , testCase "da-prefix + roots2 + ra-subfix with vowel" $
            parseRaws (pConstraint12) "དཀྲོ" @?= Right ["ད", "ཀ", "ྲ", "ོ"]
        , testCase "ba-prefix + roots3 + ya-subfix without vowel" $
            parseRaws (pConstraint12) "བཀྱ" @?= Right ["བ", "ཀ", "ྱ"]
        , testCase "ba-prefix + roots3 + ya-subfix with vowel" $
            parseRaws (pConstraint12) "བཀྱུ" @?= Right ["བ", "ཀ", "ྱ", "ུ"]
        , testCase "ba-prefix + roots4 + ra-subfix without vowel" $
            parseRaws (pConstraint12) "བཀྲ" @?= Right ["བ", "ཀ", "ྲ"]
        , testCase "ba-prefix + roots4 + ra-subfix with vowel" $
            parseRaws (pConstraint12) "བཀྲོ" @?= Right ["བ", "ཀ", "ྲ", "ོ"]
        , testCase "ba-prefix + roots5 + la-subfix without vowel" $
            parseRaws (pConstraint12) "བཀླ" @?= Right ["བ", "ཀ", "ླ"]
        , testCase "ba-prefix + roots5 + la-subfix with vowel" $
            parseRaws (pConstraint12) "བཀློ" @?= Right ["བ", "ཀ", "ླ", "ོ"]
        , testCase "ma-prefix + roots6 + ya-subfix without vowel" $
            parseRaws (pConstraint12) "མཁྱ" @?= Right ["མ", "ཁ", "ྱ"]
        , testCase "ma-prefix + roots6 + ra-subfix without vowel" $
            parseRaws (pConstraint12) "མཁྲ" @?= Right ["མ", "ཁ", "ྲ"]
        , testCase "a-prefix + roots7 + ya-subfix without vowel" $
            parseRaws (pConstraint12) "འཁྱ" @?= Right ["འ", "ཁ", "ྱ"]
        , testCase "a-prefix + roots7 + ya-subfix with vowel" $
            parseRaws (pConstraint12) "འཁྱུ" @?= Right ["འ", "ཁ", "ྱ", "ུ"]
        , testCase "a-prefix + roots8 + ra-subfix without vowel" $
            parseRaws (pConstraint12) "འཁྲ" @?= Right ["འ", "ཁ", "ྲ"]
        , testCase "a-prefix + roots8 + ra-subfix with vowel" $
            parseRaws (pConstraint12) "འཁྲོ" @?= Right ["འ", "ཁ", "ྲ", "ོ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint12) "དཀྱུ" @?= Right [RolePrefix, RoleRoot]
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
