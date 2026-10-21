-- Ожидания: книга (Nyima Tashi), правило 14.

module Test.Grammar.Constraint.Constraint14 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser
    ( SpellParser
    , parseEither
    , runSpell
    )
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
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
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint14) "གཅུ" @?= Right [Prefix, Root, Vowel]
        , testCase "a stack dot after the prefix parts the stacks" $
            parseWyliePositions (pConstraint14) "g.yu"
                @?= Right [Prefix, Root, Vowel]
        , testCase "without a dot the bare y is no root here" $
            isLeft (parseWyliePositions (pConstraint14) "gyu") @?= True
        ]

parseWyliePositions ::
    SpellParser TibetanSyllable -> Text -> Either Text [Position]
parseWyliePositions p input =
    fmap (toList . fmap fst) $
        parseEither (runSpell p) (fst (tokenizeWylie input))

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
