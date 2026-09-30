module Test.Grammar.Constraint.Constraint20 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( Parser
    , Spelling (..)
    , parseEither
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
        "grammar rule 20"
        [ testCase "parses root འ" $
            parseRaws (pConstraint20 Tibetan) "འ" @?= Right ["འ"]
        , testCase "parses root འ with vowel" $
            parseRaws (pConstraint20 Tibetan) "འི" @?= Right ["འ", "ི"]
        , testCase "parses root འ with subroot ང" $
            parseRaws (pConstraint20 Tibetan) "འྔ" @?= Right ["འ", "ྔ"]
        , testCase "parses root འ with subroot མ" $
            parseRaws (pConstraint20 Tibetan) "འྨ" @?= Right ["འ", "ྨ"]
        , testCase "rejects root ཀ" $
            isLeft (parseRaws (pConstraint20 Tibetan) "ཀ") @?= True
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint20 Tibetan) "འོ" @?= Right [Root, Vowel]
        ]

parseRaws :: Parser TibetanSyllable -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanSyllable -> Text -> Either Text [Position]
parsePositions p input =
    fmap (toList . fmap fst) $
        parseEither p (fst (tokenizeUnicode input))
