module Test.Grammar.Constraint.Constraint12 (tests) where

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
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar rule 12"
        [ testCase "da-prefix + roots1 + ya-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "དཀྱ" @?= Right ["ད", "ཀ", "ྱ"]
        , testCase "da-prefix + roots1 + ya-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "དཀྱུ" @?= Right ["ད", "ཀ", "ྱ", "ུ"]
        , testCase "da-prefix + roots2 + ra-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "དཀྲ" @?= Right ["ད", "ཀ", "ྲ"]
        , testCase "da-prefix + roots2 + ra-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "དཀྲོ" @?= Right ["ད", "ཀ", "ྲ", "ོ"]
        , testCase "ba-prefix + roots3 + ya-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀྱ" @?= Right ["བ", "ཀ", "ྱ"]
        , testCase "ba-prefix + roots3 + ya-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀྱུ" @?= Right ["བ", "ཀ", "ྱ", "ུ"]
        , testCase "ba-prefix + roots4 + ra-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀྲ" @?= Right ["བ", "ཀ", "ྲ"]
        , testCase "ba-prefix + roots4 + ra-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀྲོ" @?= Right ["བ", "ཀ", "ྲ", "ོ"]
        , testCase "ba-prefix + roots5 + la-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀླ" @?= Right ["བ", "ཀ", "ླ"]
        , testCase "ba-prefix + roots5 + la-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "བཀློ" @?= Right ["བ", "ཀ", "ླ", "ོ"]
        , testCase "ma-prefix + roots6 + ya-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "མཁྱ" @?= Right ["མ", "ཁ", "ྱ"]
        , testCase "ma-prefix + roots6 + ra-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "མཁྲ" @?= Right ["མ", "ཁ", "ྲ"]
        , testCase "a-prefix + roots7 + ya-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "འཁྱ" @?= Right ["འ", "ཁ", "ྱ"]
        , testCase "a-prefix + roots7 + ya-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "འཁྱུ" @?= Right ["འ", "ཁ", "ྱ", "ུ"]
        , testCase "a-prefix + roots8 + ra-subfix without vowel" $
            parseRaws (pConstraint12 Tibetan) "འཁྲ" @?= Right ["འ", "ཁ", "ྲ"]
        , testCase "a-prefix + roots8 + ra-subfix with vowel" $
            parseRaws (pConstraint12 Tibetan) "འཁྲོ" @?= Right ["འ", "ཁ", "ྲ", "ོ"]
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint12 Tibetan) "དཀྱུ"
                @?= Right [Prefix, Root, Subfix, Vowel]
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
