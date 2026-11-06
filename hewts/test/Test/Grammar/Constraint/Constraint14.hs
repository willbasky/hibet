-- Ожидания: книга (Nyima Tashi), правило 14.

module Test.Grammar.Constraint.Constraint14 (tests) where

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
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar rule 14"
        [ testCase "ga-prefix + roots1 without vowel" $
            parseRaws (pConstraint14) "གཅ" @?= Right ["ག", "ཅ"]
        , testCase "ga-prefix + roots1 with vowel" $
            parseRaws (pConstraint14) "གཅུ" @?= Right ["ག", "ཅ", "ུ"]
        , testCase "da-prefix + roots2 without vowel" $
            parseRaws (pConstraint14) "དཀ" @?= Right ["ད", "ཀ"]
        , testCase "da-prefix + roots2 with vowel" $
            parseRaws (pConstraint14) "དཀུ" @?= Right ["ད", "ཀ", "ུ"]
        , testCase "ba-prefix + roots3 without vowel" $
            parseRaws (pConstraint14) "བཀ" @?= Right ["བ", "ཀ"]
        , testCase "ba-prefix + roots3 with vowel" $
            parseRaws (pConstraint14) "བཀུ" @?= Right ["བ", "ཀ", "ུ"]
        , testCase "ma-prefix + roots4 without vowel" $
            parseRaws (pConstraint14) "མཁ" @?= Right ["མ", "ཁ"]
        , testCase "ma-prefix + roots4 with vowel" $
            parseRaws (pConstraint14) "མཁུ" @?= Right ["མ", "ཁ", "ུ"]
        , testCase "a-prefix + roots5 without vowel" $
            parseRaws (pConstraint14) "འཁ" @?= Right ["འ", "ཁ"]
        , testCase "a-prefix + roots5 with vowel" $
            parseRaws (pConstraint14) "འཁུ" @?= Right ["འ", "ཁ", "ུ"]
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint14) "གཅུ" @?= Right [RolePrefix, RoleRoot]
        , testCase "a stack dot after the prefix parts the stacks" $
            parseWylieRoles (pConstraint14) "g.yu"
                @?= Right [RolePrefix, RoleRoot]
        , testCase "without a dot the bare y is no root here" $
            isLeft (parseWylieRoles (pConstraint14) "gyu") @?= True
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

-- | The same roles, read from a Wylie run.
parseWylieRoles :: SpellParser TibetanSyllable -> Text -> Either Text [Role]
parseWylieRoles p input =
    fmap (map stRole . toList) $
        parseEither (runSpell (p *> takeStacks)) (fst (tokenizeWylie input))
