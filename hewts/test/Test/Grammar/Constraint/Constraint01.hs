module Test.Grammar.Constraint.Constraint01 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( Parser
    , Spelling (..)
    , parseEither
    )
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar rule 01"
        [ testCase "pConstraint01 parses root only" $
            parseRaws (pConstraint01 Tibetan) "ས" @?= Right ["ས"]
        , testCase "pConstraint01 parses root+vowel" $
            parseRaws (pConstraint01 Tibetan) "སུ" @?= Right ["ས", "ུ"]
        , testCase "(pConstraint01WithLong Tibetan) parses root+regular vowel" $
            parseRaws (pConstraint01WithLong Tibetan) "དུ" @?= Right ["ད", "ུ"]
        , testCase "(pConstraint01WithLong Tibetan) parses root+long A" $
            parseRaws (pConstraint01WithLong Tibetan) "སཱ" @?= Right ["ས", "ཱ"]
        , testCase "(pConstraint01Sanskrit Tibetan) parses sanskrit root only" $
            parseRaws (pConstraint01Sanskrit Tibetan) "ཌ" @?= Right ["ཌ"]
        , testCase "(pConstraint01Sanskrit Tibetan) parses sanskrit root+vowel" $
            parseRaws (pConstraint01Sanskrit Tibetan) "ཌོ" @?= Right ["ཌ", "ོ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint01 Tibetan) "སུ" @?= Right [Root, Vowel]
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
