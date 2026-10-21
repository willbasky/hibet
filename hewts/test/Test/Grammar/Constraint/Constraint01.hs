-- Ожидания: книга (Nyima Tashi), правило 01.

module Test.Grammar.Constraint.Constraint01 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
    , parseEither
    , runSpell
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
            parseRaws (pConstraint01) "ས" @?= Right ["ས"]
        , testCase "pConstraint01 parses root+vowel" $
            parseRaws (pConstraint01) "སུ" @?= Right ["ས", "ུ"]
        , testCase "(pConstraint01WithLong) parses root+regular vowel" $
            parseRaws (pConstraint01WithLong) "དུ" @?= Right ["ད", "ུ"]
        , testCase "(pConstraint01WithLong) parses root+long A" $
            parseRaws (pConstraint01WithLong) "སཱ" @?= Right ["ས", "ཱ"]
        , testCase "(pConstraint01Sanskrit) parses sanskrit root only" $
            parseRaws (pConstraint01Sanskrit) "ཌ" @?= Right ["ཌ"]
        , testCase "(pConstraint01Sanskrit) parses sanskrit root+vowel" $
            parseRaws (pConstraint01Sanskrit) "ཌོ" @?= Right ["ཌ", "ོ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint01) "སུ" @?= Right [Root, Vowel]
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
