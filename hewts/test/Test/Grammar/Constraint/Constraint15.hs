module Test.Grammar.Constraint.Constraint15 (tests) where

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
        "grammar rule 15"
        [ testCase "parses suffix ག" $
            parseRaws (pConstraint15 Tibetan) "ག" @?= Right ["ག"]
        , testCase "parses suffix ང" $
            parseRaws (pConstraint15 Tibetan) "ང" @?= Right ["ང"]
        , testCase "parses suffix ད" $
            parseRaws (pConstraint15 Tibetan) "ད" @?= Right ["ད"]
        , testCase "parses suffix ན" $
            parseRaws (pConstraint15 Tibetan) "ན" @?= Right ["ན"]
        , testCase "parses suffix བ" $
            parseRaws (pConstraint15 Tibetan) "བ" @?= Right ["བ"]
        , testCase "parses suffix མ" $
            parseRaws (pConstraint15 Tibetan) "མ" @?= Right ["མ"]
        , testCase "parses suffix འ" $
            parseRaws (pConstraint15 Tibetan) "འ" @?= Right ["འ"]
        , testCase "parses suffix ར" $
            parseRaws (pConstraint15 Tibetan) "ར" @?= Right ["ར"]
        , testCase "parses suffix ལ" $
            parseRaws (pConstraint15 Tibetan) "ལ" @?= Right ["ལ"]
        , testCase "parses suffix ས" $
            parseRaws (pConstraint15 Tibetan) "ས" @?= Right ["ས"]
        , testCase "rejects non-suffix consonant ཀ" $
            isLeft (parseRaws (pConstraint15 Tibetan) "ཀ") @?= True
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint15 Tibetan) "ག" @?= Right [Suffix]
        ]

parseRaws :: Parser TibetanWord -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanWord -> Text -> Either Text [Position]
parsePositions p input = fmap (toList . fmap fst) $ parseEither p (fst (tokenizeUnicode input))
