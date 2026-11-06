-- Ожидания: книга (Nyima Tashi), правило 16.

module Test.Grammar.Constraint.Constraint16 (tests) where

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
        "grammar rule 16"
        [ testCase "parses suffix ན before postfix ད" $
            parseRaws (pConstraint16Da) "ན" @?= Right ["ན"]
        , testCase "parses suffix ར before postfix ད" $
            parseRaws (pConstraint16Da) "ར" @?= Right ["ར"]
        , testCase "parses suffix ལ before postfix ད" $
            parseRaws (pConstraint16Da) "ལ" @?= Right ["ལ"]
        , testCase "rejects ག for postfix ད rule" $
            isLeft (parseRaws (pConstraint16Da) "ག") @?= True
        , testCase "parses suffix ག before postfix ས" $
            parseRaws (pConstraint16Sa) "ག" @?= Right ["ག"]
        , testCase "parses suffix ང before postfix ས" $
            parseRaws (pConstraint16Sa) "ང" @?= Right ["ང"]
        , testCase "parses suffix བ before postfix ས" $
            parseRaws (pConstraint16Sa) "བ" @?= Right ["བ"]
        , testCase "parses suffix མ before postfix ས" $
            parseRaws (pConstraint16Sa) "མ" @?= Right ["མ"]
        , testCase "rejects ན for postfix ས rule" $
            isLeft (parseRaws (pConstraint16Sa) "ན") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint16Da) "ན" @?= Right [RoleSuffix]
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
