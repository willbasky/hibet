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
        , wave344
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
        , testCase "grglam blames the subscript and the gated stack" $
            rendered "grglam"
                @?= Right
                    [ "line 1: \"grglam\": The prefix \"g\" does not allow \"r\" after it."
                    , "line 1: \"grglam\": The superfix \"r\" does not occur above \"g\" with \"l\" below it."
                    ]
        , testCase "g....yag blames the stack dot" $
            rendered "g....yag"
                @?= Right
                    [ "line 1: \"g....yag\": The prefix \"g\" does not allow \".\" after it."
                    ]
        ]

-- | The wave-3.4.4 legality windows: the second caret, two finals of one
-- class, the join after a placed vowel, the superfix combination tables, the
-- vowel after a prefix, and the suffix-position pair rule of the word tail.
wave344 :: TestTree
wave344 =
    testGroup
        "the stack and the word tail (3.4.4)"
        [ testCase "a second caret is blamed" $
            rendered "g^r^a"
                @?= Right
                    ["line 1: \"g^r^a\": The caret \"^\" occurs more than once in this stack."]
        , testCase "two finals of one class are blamed" $
            rendered "kaMM"
                @?= Right ["line 1: \"kaMM\": Two finals of the \"M\" class in one stack."]
        , testCase "a join after the vowel subjoins its consonant" $
            rendered "ku+k"
                @?= Right
                    [ "line 1: \"ku+k\": The join \"+\" places \"k\" below a stack that already has its vowel."
                    ]
        , testCase "a join after the vowel that brings a vowel stays legal" $
            rendered "ku+e" @?= Right []
        , testCase "l takes no subjoined combinations" $
            rendered "lkya"
                @?= Right
                    [ "line 1: \"lkya\": The superfix \"l\" does not occur above \"k\" with \"y\" below it."
                    ]
        , testCase "r over k+w is no combination" $
            rendered "rkwa"
                @?= Right
                    [ "line 1: \"rkwa\": The superfix \"r\" does not occur above \"k\" with \"w\" below it."
                    ]
        , testCase "r over a letter outside its roots is no combination" $
            rendered "rpa"
                @?= Right ["line 1: \"rpa\": The superfix \"r\" does not occur above \"p\"."]
        , testCase "s over g+r+w is no combination (the reference's domain)" $
            rendered "sgrwa"
                @?= Right
                    [ "line 1: \"sgrwa\": The superfix \"s\" does not occur above \"g\" with \"rw\" below it."
                    ]
        , testCase "s over k+r stays legal" $
            rendered "skra" @?= Right []
        , testCase "a prefix whose stack never reaches a vowel" $
            rendered "bk"
                @?= Right ["line 1: \"bk\": The stack the prefix \"b\" leads carries no vowel."]
        , testCase "a prefix whose stack reaches a vowel stays silent" $
            rendered "bka" @?= Right []
        , testCase "a postfix over the wrong first suffix" $
            rendered "kabd"
                @?= Right ["line 1: \"kabd\": The second suffix \"d\" does not occur after \"b\"."]
        , testCase "a consonant in the second suffix slot" $
            rendered "thabg"
                @?= Right ["line 1: \"thabg\": The consonant \"g\" cannot be a second suffix."]
        , testCase "a consonant after a legal second suffix" $
            rendered "dagsg"
                @?= Right ["line 1: \"dagsg\": The consonant \"g\" cannot follow a second suffix."]
        , testCase "the second-suffix pair of rule 4.16 stays silent" $
            rendered "thabs" @?= Right []
        , testCase "a retroflex root under a superfix is no 4.8 combination" $
            rendered "rTa"
                @?= Right
                    ["line 1: \"rTa\": The superfix \"r\" does not occur above \"T\"."]
        , testCase "l over a retroflex root is no 4.8 combination" $
            rendered "lTa"
                @?= Right
                    ["line 1: \"lTa\": The superfix \"l\" does not occur above \"T\"."]
        , testCase "s over a retroflex root is no 4.8 combination" $
            rendered "sDa"
                @?= Right
                    ["line 1: \"sDa\": The superfix \"s\" does not occur above \"D\"."]
        , testCase "a plain root under the superfix r stays legal (the 4.8 tables)" $
            rendered "rtaba" @?= Right []
        , testCase "a plain root under the superfix l stays legal (the 4.8 tables)" $
            rendered "ltaba" @?= Right []
        , testCase "a plain root under the superfix s stays legal (the 4.8 tables)" $
            rendered "staba" @?= Right []
        , testCase "a retroflex second suffix is no 4.16 pair" $
            rendered "kaND"
                @?= Right
                    ["line 1: \"kaND\": The consonant \"D\" cannot be a second suffix."]
        , testCase "d after the plain n stays legal (the 4.16 tables)" $
            rendered "kand" @?= Right []
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
        , testCase "the 3.4.4 warnings carry their codes and severity" $
            recordsOf "rkwa"
                @?= Right [(IllegalSuperfixCombination, SevWarning)]
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
