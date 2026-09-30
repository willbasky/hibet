module Test.Grammar.Constraint.Constraint18 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
    , Spelling (..)
    , parseEither
    , runSpell
    )
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
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
            parseRaws (pConstraint18 Tibetan) "ཧྥ" @?= Right ["ཧ", "ྥ"]
        , testCase "parses root ཧ with vowel above subroot ཕ" $
            parseRaws (pConstraint18 Tibetan) "ཧྥི" @?= Right ["ཧ", "ྥ", "ི"]
        , testCase "rejects wrong subroot above ཧ" $
            isLeft (parseRaws (pConstraint18 Tibetan) "ཧྲ") @?= True
        , testCase "rejects root ཀ above subroot ཕ" $
            isLeft (parseRaws (pConstraint18 Tibetan) "ཀྥ") @?= True
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint18 Tibetan) "ཧྥི" @?= Right [Root, Root, Vowel]
        ]

parseRaws :: SpellParser TibetanSyllable -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither (runSpell p) (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: SpellParser TibetanSyllable -> Text -> Either Text [Position]
parsePositions p input =
    fmap (toList . fmap fst) $
        parseEither (runSpell p) (fst (tokenizeUnicode input))
