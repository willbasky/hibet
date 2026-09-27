-- | The spelling checker's machinery as of 3.4.1: what counts as a word and
-- that the (still empty) rule set stays silent. The rules themselves arrive in
-- 3.4.2+, each verified here with its own corpus-facing cases.
module Test.Grammar.Legality (tests) where

import Convert.Diagnostic (diagnosticList)
import Convert.Grammar.Legality (checkedStream, legality, splitWords)
import Convert.Grammar.Parser (Spelling (..), parseEither)
import Convert.Sentence (pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar legality (channel)"
        [ testCase "words split at whitespace" $
            rawWords "ka ba" @?= Right [["k", "a"], ["b", "a"]]
        , testCase "the caret stays inside its word" $
            rawWords "gra^ ba" @?= Right [["g", "r", "a", "^"], ["b", "a"]]
        , testCase "the stack dot stays inside its word" $
            rawWords "g.yag" @?= Right [["g", ".", "y", "a", "g"]]
        , testCase "the plus stays inside its word" $
            rawWords "sat+t+wa ba"
                @?= Right [["s", "a", "t", "+", "t", "+", "w", "a"], ["b", "a"]]
        , testCase "the checker keeps a plain sentence silent" $
            warningCount "ka ba" @?= Right 0
        , testCase "the checker keeps a stack word silent" $
            warningCount "g.yag" @?= Right 0
        ]

-- | The raw spellings of each word of a Wylie input, word = run of tokens
-- between whitespace or punctuation (the wave-3.4 word boundary).
rawWords :: Text -> Either Text [[Text]]
rawWords input = do
    let tokens = fst (tokenizeWylie input)
    items <- parseEither (pSentence Wylie) tokens
    pure
        [ [tokenRaw t | (_, t) <- word] | word <- splitWords (checkedStream tokens items)
        ]

-- | How many spelling warnings a Wylie input earns today; zero until 3.4.2.
warningCount :: Text -> Either Text Int
warningCount input = do
    let tokens = fst (tokenizeWylie input)
    items <- parseEither (pSentence Wylie) tokens
    pure (length (diagnosticList (legality tokens items)))
