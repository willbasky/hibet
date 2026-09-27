-- | The spelling checker as of 3.4.2: what counts as a word, and the prefix
-- position with its two warnings (section 4.2). Each rule lands with its own
-- corpus-facing cases.
module Test.Grammar.Legality (tests) where

import Convert (SpellItem (..), legality)
import Convert.Diagnostic (diagnosticList, renderDiagnostics)
import Convert.Grammar.Parser (Spelling (..), isPunctuationLike, parseEither)
import Convert.Sentence (Syllable (..), pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "grammar legality"
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
        , testCase "the checker keeps a legal prefix silent" $
            rendered "bka" @?= Right []
        , testCase "the checker keeps a vowel-next word silent" $
            rendered "da" @?= Right []
        , testCase "the checker keeps a superfix stack silent" $
            rendered "rka" @?= Right []
        , testCase "the checker keeps a stacked root silent" $
            rendered "sgra" @?= Right []
        , testCase "tgra: the head is no prefix letter" $
            rendered "tgra"
                @?= Right ["line 1: \"tgra\": Invalid prefix consonant: \"t\"."]
        , testCase "c: the head is no prefix letter" $
            rendered "c"
                @?= Right ["line 1: \"c\": Invalid prefix consonant: \"c\"."]
        , testCase "pgru: the head is no prefix letter" $
            rendered "pgru"
                @?= Right ["line 1: \"pgru\": Invalid prefix consonant: \"p\"."]
        , testCase "srba: a backtracked superfix head is no prefix letter" $
            rendered "srba"
                @?= Right ["line 1: \"srba\": Invalid prefix consonant: \"s\"."]
        , testCase "grla: a prefix cannot lead its subscript" $
            rendered "grla"
                @?= Right ["line 1: \"grla\": Prefix \"g\" does not occur before \"r\"."]
        , testCase "bdza: a prefix cannot lead its root" $
            rendered "bdza"
                @?= Right ["line 1: \"bdza\": Prefix \"b\" does not occur before \"dz\"."]
        , testCase "grglam: the prefix part blames the subscript" $
            rendered "grglam"
                @?= Right ["line 1: \"grglam\": Prefix \"g\" does not occur before \"r\"."]
        , testCase "g....yag: the stack dot stays the blamed next" $
            rendered "g....yag"
                @?= Right ["line 1: \"g....yag\": Prefix \"g\" does not occur before \".\"."]
        ]

-- | The raw spellings of each syllable of a Wylie input, minus the syllables'
-- trailing boundaries (the wave-3.4 word boundary lives inside every run).
rawWords :: Text -> Either Text [[Text]]
rawWords input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure
        [ [tokenRaw t | (_, t) <- toList (syllableTokens s), not (isPunctuationLike t)]
        | SyllableItem s <- items
        ]

-- | How many spelling warnings a Wylie input earns today; zero until 3.4.2.
warningCount :: Text -> Either Text Int
warningCount input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure (length (diagnosticList (legality items)))

-- | The checker's messages, rendered the way the reference renders them
-- (@line N: "word": message@). Drop-in for the corpus vectors.
rendered :: Text -> Either Text [Text]
rendered input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure (renderDiagnostics input (legality items))
