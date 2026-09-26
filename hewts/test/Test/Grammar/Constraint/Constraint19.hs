module Test.Grammar.Constraint.Constraint19 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser (Parser, Spelling (..), parseEither)
import Convert.Grammar.Word (Position (..), TibetanWord)
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
            parseRaws (pConstraint19 Tibetan) "ཧྥག" @?= Right ["ཧ", "ྥ", "ག"]
        , testCase "parses struct with vowel and suffix ས" $
            parseRaws (pConstraint19 Tibetan) "ཧྥིས" @?= Right ["ཧ", "ྥ", "ི", "ས"]
        , testCase "rejects suffix ཀ not in grammar 15" $
            isLeft (parseRaws (pConstraint19 Tibetan) "ཧྥཀ") @?= True
        , testCase "rejects missing suffix" $
            isLeft (parseRaws (pConstraint19 Tibetan) "ཧྥ") @?= True
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint19 Tibetan) "ཧྥིས"
                @?= Right [Root, Root, Vowel, Suffix]
        ]

parseRaws :: Parser TibetanWord -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanWord -> Text -> Either Text [Position]
parsePositions p input = fmap (toList . fmap fst) $ parseEither p (fst (tokenizeUnicode input))
