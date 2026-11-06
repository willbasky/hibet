-- Ожидания: книга (Nyima Tashi), правило 19.

module Test.Grammar.Constraint.Constraint19 (tests) where

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
        "grammar rule 19"
        [ testCase "parses root ཧ subroot ཕ with suffix ག" $
            parseRaws (pConstraint19) "ཧྥག" @?= Right ["ཧ", "ྥ", "ག"]
        , testCase "parses struct with vowel and suffix ས" $
            parseRaws (pConstraint19) "ཧྥིས" @?= Right ["ཧ", "ྥ", "ི", "ས"]
        , testCase "rejects suffix ཀ not in grammar 15" $
            isLeft (parseRaws (pConstraint19) "ཧྥཀ") @?= True
        , testCase "rejects missing suffix" $
            isLeft (parseRaws (pConstraint19) "ཧྥ") @?= True
        , testCase "the rule builds the stacks of the word" $
            parseRoles (pConstraint19) "ཧྥིས" @?= Right [RoleRoot, RoleSuffix]
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
