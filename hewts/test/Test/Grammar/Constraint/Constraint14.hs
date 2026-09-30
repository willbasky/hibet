module Test.Grammar.Constraint.Constraint14 (tests) where

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
        "grammar rule 14"
        [ testCase "ga-prefix + roots1 without vowel" $
            parseRaws (pConstraint14 Tibetan) "གཅ" @?= Right ["ག", "ཅ"]
        , testCase "ga-prefix + roots1 with vowel" $
            parseRaws (pConstraint14 Tibetan) "གཅུ" @?= Right ["ག", "ཅ", "ུ"]
        , testCase "da-prefix + roots2 without vowel" $
            parseRaws (pConstraint14 Tibetan) "དཀ" @?= Right ["ད", "ཀ"]
        , testCase "da-prefix + roots2 with vowel" $
            parseRaws (pConstraint14 Tibetan) "དཀུ" @?= Right ["ད", "ཀ", "ུ"]
        , testCase "ba-prefix + roots3 without vowel" $
            parseRaws (pConstraint14 Tibetan) "བཀ" @?= Right ["བ", "ཀ"]
        , testCase "ba-prefix + roots3 with vowel" $
            parseRaws (pConstraint14 Tibetan) "བཀུ" @?= Right ["བ", "ཀ", "ུ"]
        , testCase "ma-prefix + roots4 without vowel" $
            parseRaws (pConstraint14 Tibetan) "མཁ" @?= Right ["མ", "ཁ"]
        , testCase "ma-prefix + roots4 with vowel" $
            parseRaws (pConstraint14 Tibetan) "མཁུ" @?= Right ["མ", "ཁ", "ུ"]
        , testCase "a-prefix + roots5 without vowel" $
            parseRaws (pConstraint14 Tibetan) "འཁ" @?= Right ["འ", "ཁ"]
        , testCase "a-prefix + roots5 with vowel" $
            parseRaws (pConstraint14 Tibetan) "འཁུ" @?= Right ["འ", "ཁ", "ུ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint14 Tibetan) "གཅུ" @?= Right [Prefix, Root, Vowel]
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
