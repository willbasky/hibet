-- | The converter's own diagnostics (wave 3.4): the records the grammar's
-- constraint windows produce in their own windows, with our wording, our
-- codes and our channel. The expectations here are ours, not the
-- reference's - the reference comparisons cover conversion only.
module Test.Diagnostic (tests) where

import Convert
    ( SpellItem (..)
    , legality
    )
import Convert.Diagnostic
    ( Diagnostic (..)
    , DiagnosticCode (..)
    , Severity (..)
    , diagnosticList
    , renderDiagnostics
    )
import Convert.Grammar.Parser
    ( Spelling (..)
    , isPunctuationLike
    , parseEither
    )
import Convert.Sentence (Syllable (..), pSentence)
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "diagnostics"
        [ runWalk
        , stillSilent
        , invalidPrefixWording
        , prefixCannotLeadWording
        , records
        ]

-- | The walk sees the same syllable runs the checker used to: a run is the
-- tokens up to the next boundary, and a stack dot, a caret or a plus stays
-- inside it.
runWalk :: TestTree
runWalk =
    testGroup
        "the walk sees the whole run"
        [ testCase "words split at whitespace" $
            rawWords "ka ba" @?= Right [["k", "a"], ["b", "a"]]
        , testCase "the caret stays inside its word" $
            rawWords "gra^ ba" @?= Right [["g", "r", "a", "^"], ["b", "a"]]
        , testCase "the stack dot stays inside its word" $
            rawWords "g.yag" @?= Right [["g", ".", "y", "a", "g"]]
        , testCase "the plus stays inside its word" $
            rawWords "sat+t+wa ba"
                @?= Right [["s", "a", "t", "+", "t", "+", "w", "a"], ["b", "a"]]
        ]

-- | The channel stays silent where nothing is wrong.
stillSilent :: TestTree
stillSilent =
    testGroup
        "silent where right"
        [ testCase "a plain sentence" $ rendered "ka ba" @?= Right []
        , testCase "a legal prefix" $ rendered "bka" @?= Right []
        , testCase "a vowel-next word" $ rendered "da" @?= Right []
        , testCase "a superfix stack" $ rendered "rka" @?= Right []
        , testCase "a stacked root" $ rendered "sgra" @?= Right []
        ]

-- | The head window, first warning: a consonant in the PREFIX state that is
-- no prefix letter at all.
invalidPrefixWording :: TestTree
invalidPrefixWording =
    testGroup
        "the head is no prefix letter"
        [ testCase "tgra" $
            rendered "tgra"
                @?= Right ["line 1: \"tgra\": The letter \"t\" cannot be a prefix."]
        , testCase "c" $
            rendered "c"
                @?= Right ["line 1: \"c\": The letter \"c\" cannot be a prefix."]
        , testCase "pgru" $
            rendered "pgru"
                @?= Right ["line 1: \"pgru\": The letter \"p\" cannot be a prefix."]
        , testCase "srba backtracks to a bare head" $
            rendered "srba"
                @?= Right ["line 1: \"srba\": The letter \"s\" cannot be a prefix."]
        ]

-- | The head window, second warning: a prefix letter leads a letter its table
-- (section 4.2) does not allow.
prefixCannotLeadWording :: TestTree
prefixCannotLeadWording =
    testGroup
        "a prefix cannot lead the next letter"
        [ testCase "grla blames the subscript" $
            rendered "grla"
                @?= Right
                    [ "line 1: \"grla\": The prefix \"g\" does not allow \"r\" after it."
                    ]
        , testCase "bdza blames the root" $
            rendered "bdza"
                @?= Right
                    [ "line 1: \"bdza\": The prefix \"b\" does not allow \"dz\" after it."
                    ]
        , testCase "grglam blames the subscript" $
            rendered "grglam"
                @?= Right
                    [ "line 1: \"grglam\": The prefix \"g\" does not allow \"r\" after it."
                    ]
        , testCase "g....yag blames the stack dot" $
            rendered "g....yag"
                @?= Right
                    [ "line 1: \"g....yag\": The prefix \"g\" does not allow \".\" after it."
                    ]
        ]

-- | The diagnostic record: the code, the severity and the quoted word, so the
-- UI maps codes to help text without parsing messages.
records :: TestTree
records =
    testGroup
        "records"
        [ testCase "invalid-prefix records carry their code and severity" $
            recordsOf "tgra"
                @?= Right
                    [ (InvalidPrefix, SevWarning)
                    ]
        , testCase "prefix-cannot-lead records carry their code and severity" $
            recordsOf "grla"
                @?= Right
                    [ (PrefixCannotLead, SevWarning)
                    ]
        , testCase "the word span is quoted for both warnings" $
            wordsOf "tgra"
                @?= Right ["line 1: \"tgra\""]
        , testCase "silent runs carry no records at all" $
            recordsOf "bka" @?= Right []
        ]

-- | The raw spellings of each syllable of a Wylie input, minus the
-- syllables' trailing boundaries (the wave-3.4 word boundary lives inside
-- every run).
rawWords :: Text -> Either Text [[Text]]
rawWords input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure
        [ [tokenRaw t | (_, t) <- toList (syllableTokens s), not (isPunctuationLike t)]
        | SyllableItem s <- items
        ]

-- | The walk's messages, rendered on the converter's channel
-- (@line N: "word": message@).
rendered :: Text -> Either Text [Text]
rendered input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure (renderDiagnostics input (legality items))

-- | The codes and severities of the walk's messages, in order.
recordsOf :: Text -> Either Text [(DiagnosticCode, Severity)]
recordsOf input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure
        [ (diagCode d, diagSeverity d)
        | d <- diagnosticList (legality items)
        ]

-- | The quoted-word part of each message (the @line N: "word"@ prefix of the
-- rendered form), so a typo in the span logic shows up independently of the
-- wording.
wordsOf :: Text -> Either Text [Text]
wordsOf input = do
    items <- parseEither (pSentence Wylie) (fst (tokenizeWylie input))
    pure (map wordPrefix (renderDiagnostics input (legality items)))
    where
        -- @line N: "word": message@: the first two @:@-separated segments are
        -- the line and the quoted word; our messages use no colon in between.
        wordPrefix m =
            case T.splitOn ":" m of
                lineNo : wordPart : _ -> lineNo <> ":" <> wordPart
                _ -> m
