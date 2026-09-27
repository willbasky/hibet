module Test.Grammar.Constraint.Constraint16 (tests) where

import Convert.Grammar.Constraint
import Convert.Grammar.Parser (Parser, Spelling (..), parseEither)
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
        "grammar rule 16"
        [ testCase "parses suffix ན before postfix ད" $
            parseRaws (pConstraint16Da Tibetan) "ན" @?= Right ["ན"]
        , testCase "parses suffix ར before postfix ད" $
            parseRaws (pConstraint16Da Tibetan) "ར" @?= Right ["ར"]
        , testCase "parses suffix ལ before postfix ད" $
            parseRaws (pConstraint16Da Tibetan) "ལ" @?= Right ["ལ"]
        , testCase "rejects ག for postfix ད rule" $
            isLeft (parseRaws (pConstraint16Da Tibetan) "ག") @?= True
        , testCase "parses suffix ག before postfix ས" $
            parseRaws (pConstraint16Sa Tibetan) "ག" @?= Right ["ག"]
        , testCase "parses suffix ང before postfix ས" $
            parseRaws (pConstraint16Sa Tibetan) "ང" @?= Right ["ང"]
        , testCase "parses suffix བ before postfix ས" $
            parseRaws (pConstraint16Sa Tibetan) "བ" @?= Right ["བ"]
        , testCase "parses suffix མ before postfix ས" $
            parseRaws (pConstraint16Sa Tibetan) "མ" @?= Right ["མ"]
        , testCase "rejects ན for postfix ས rule" $
            isLeft (parseRaws (pConstraint16Sa Tibetan) "ན") @?= True
        , testCase "marks each letter of the word" $
            parsePositions (pConstraint16Da Tibetan) "ན" @?= Right [Suffix]
        ]

parseRaws :: Parser TibetanSyllable -> Text -> Either Text [Text]
parseRaws p input =
    fmap (toList . fmap (tokenRaw . snd)) $
        parseEither p (fst (tokenizeUnicode input))

-- | The positions the rule gives each letter of the word: the marks the
-- renderer will read in wave 3, step 3.3.
parsePositions :: Parser TibetanSyllable -> Text -> Either Text [Position]
parsePositions p input = fmap (toList . fmap fst) $ parseEither p (fst (tokenizeUnicode input))
