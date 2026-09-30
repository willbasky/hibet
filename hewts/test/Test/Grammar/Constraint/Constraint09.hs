module Test.Grammar.Constraint.Constraint09 (tests) where

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
        "grammar rule 09"
        [ testCase "wa-subfix without vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྭ" @?= Right ["ཀ", "ྭ"]
        , testCase "wa-subfix with vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྭུ" @?= Right ["ཀ", "ྭ", "ུ"]
        , testCase "ya-subfix without vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྱ" @?= Right ["ཀ", "ྱ"]
        , testCase "ya-subfix with vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྱི" @?= Right ["ཀ", "ྱ", "ི"]
        , testCase "ra-subfix without vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྲ" @?= Right ["ཀ", "ྲ"]
        , testCase "ra-subfix with vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀྲེ" @?= Right ["ཀ", "ྲ", "ེ"]
        , testCase "la-subfix without vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀླ" @?= Right ["ཀ", "ླ"]
        , testCase "la-subfix with vowel" $
            parseRaws (pConstraint09 Tibetan) "ཀླེ" @?= Right ["ཀ", "ླ", "ེ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint09 Tibetan) "ཀྭུ" @?= Right [Root, Subfix, Vowel]
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
