-- Ожидания: книга (Nyima Tashi), правило 15.

module Test.Grammar.Constraint.Constraint15 (tests) where

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
        "grammar rule 15"
        [ testCase "parses suffix ག" $
            parseRaws (pConstraint15) "ག" @?= Right ["ག"]
        , testCase "parses suffix ང" $
            parseRaws (pConstraint15) "ང" @?= Right ["ང"]
        , testCase "parses suffix ད" $
            parseRaws (pConstraint15) "ད" @?= Right ["ད"]
        , testCase "parses suffix ན" $
            parseRaws (pConstraint15) "ན" @?= Right ["ན"]
        , testCase "parses suffix བ" $
            parseRaws (pConstraint15) "བ" @?= Right ["བ"]
        , testCase "parses suffix མ" $
            parseRaws (pConstraint15) "མ" @?= Right ["མ"]
        , testCase "parses suffix འ" $
            parseRaws (pConstraint15) "འ" @?= Right ["འ"]
        , testCase "parses suffix ར" $
            parseRaws (pConstraint15) "ར" @?= Right ["ར"]
        , testCase "parses suffix ལ" $
            parseRaws (pConstraint15) "ལ" @?= Right ["ལ"]
        , testCase "parses suffix ས" $
            parseRaws (pConstraint15) "ས" @?= Right ["ས"]
        , testCase "rejects non-suffix consonant ཀ" $
            isLeft (parseRaws (pConstraint15) "ཀ") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint15) "ག" @?= Right [RoleSuffix]
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
