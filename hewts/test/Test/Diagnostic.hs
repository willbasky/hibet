-- | The converter's own diagnostics (wave 3.4): the records the grammar's
-- constraint windows produce in their own windows, with our wording, our
-- codes and our channel. The expectations here are ours, not the
-- reference's - the reference comparisons cover conversion only.
module Test.Diagnostic (tests) where

import Convert
    ( OutputFormat (..)
    , SpellItem (..)
    , legality
    , renderItems
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
import Convert.Tokenizer.Unicode (tokenizeUnicode)
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
        , wave345
        , wave346
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
                    , "line 1: \"g....yag\": The stack dot \".\" joins no stack to a letter."
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

-- | The wave-3.4.5 recommendation: a syllable whose letters stand the same way
-- in two readings, where the corpus prefers one of them.
--
-- The rule speaks in Wylie, so every case here is a Wylie input: the Tibetan
-- spelling of the same syllable carries no implicit vowel and stays silent.
-- The two discriminators are covered here as well, because both are what keeps
-- the rule off the right words - a real vowel (dgi is དགི, a different
-- syllable, not དག) and a form the corpus does not list.
wave345 :: TestTree
wave345 =
    testGroup
        "the ambiguous syllable (3.4.5)"
        [ testCase "the two-letter form is recommended with the root first" $
            recommendations "dga" @?= Right ["dag"]
        , testCase "the two-letter rule is the whole suffix group" $
            recommendations "dba" @?= Right ["dab"]
        , testCase "a root letter that is no suffix letter recommends nothing" $
            recommendations "dka" @?= Right []
        , testCase "dags is recommended dgas" $
            recommendations "dags" @?= Right ["dgas"]
        , testCase "dabs is recommended dbas" $
            recommendations "dabs" @?= Right ["dbas"]
        , testCase "dams is recommended dmas" $
            recommendations "dams" @?= Right ["dmas"]
        , testCase "'ags is recommended 'gas" $
            recommendations "'ags" @?= Right ["'gas"]
        , testCase "'abs is recommended 'bas" $
            recommendations "'abs" @?= Right ["'bas"]
        , testCase "bgas is recommended bags" $
            recommendations "bgas" @?= Right ["bags"]
        , testCase "mgas is recommended mags" $
            recommendations "mgas" @?= Right ["mags"]
        , testCase "the two-letter preferred form recommends nothing" $
            recommendations "dag" @?= Right []
        , testCase "the prefix-first preferred form recommends nothing" $
            recommendations "dgas" @?= Right []
        , testCase "the postfix-first preferred form recommends nothing" $
            recommendations "bags" @?= Right []
        , testCase "the preferred reading is quiet in every other pair" $
            recommendations "dbas dmas mags 'gas 'bas" @?= Right []
        , testCase "a real vowel is no ambiguous form" $
            recommendations "dgi bgis" @?= Right []
        , testCase "a letter outside the table is no ambiguous form" $
            recommendations "dngs mngs" @?= Right []
        , testCase
            "a three-letter form not ending in the postfix letter recommends nothing"
            $ recommendations "dgam" @?= Right []
        , testCase "the Tibetan spelling of the same syllable recommends nothing" $
            tibetanRecommendations "དག་དགས་བགས་དབས" @?= Right []
        , testCase "the recommendation is quoted inside the run's word" $
            rendered "dga" @?= Right [recommendsLine "dga" "dag"]
        , testCase "each run is read and recommended on its own" $
            rendered "dga dag dags"
                @?= Right [recommendsLine "dga" "dag", recommendsLine "dags" "dgas"]
        , testCase "the recommendation carries its code and severity" $
            recordsOf "dga" @?= Right [(AmbiguousSpelling, SevWarning)]
        ]

-- | The whole run a recommendation is recorded on, as one line of the channel.
recommendsLine :: Text -> Text -> Text
recommendsLine word preferred =
    "line 1: \""
        <> word
        <> "\": The syllable is ambiguous; the preferred spelling is \""
        <> preferred
        <> "\"."

-- | The wave-3.4.6 tokenizer cases: what a word may not begin with, and the
-- dot that joins nothing.
--
-- A final mark hangs over the letter it closes, so it cannot open a run: on its
-- own it closes nothing, and the word behind it is a word of its own. The mark
-- stands exactly as it was written - the reference leaves such a character where
-- it stands too - which is what the conversion cases here pin down next to the
-- messages. A stack dot the grammar did not use is the same kind of leftover,
-- and the tail reader is the window that finds it.
wave346 :: TestTree
wave346 =
    testGroup
        "what a word may not begin with (3.4.6)"
        [ testGroup
            "a final mark cannot open a run"
            [ testCase "the mark is blamed and stands as written" $
                rendered "Mi"
                    @?= Right ["line 1: \"M\": The final \"M\" closes no letter."]
            , testCase "the word behind it is read on its own" $
                converted "Mi" @?= Right "Mཨི"
            , testCase "the mark is a run of its own" $
                wordsOf "Mi" @?= Right ["line 1: \"M\""]
            , testCase "each stray mark is blamed on its own" $
                rendered "??"
                    @?= Right
                        [ "line 1: \"?\": The final \"?\" closes no letter."
                        , "line 1: \"?\": The final \"?\" closes no letter."
                        ]
            , testCase "the marks stand as written and the word is read" $
                converted "mo . ???" @?= Right "མོ་.་???"
            , testCase "a final inside a run stays legal" $
                converted "k? oM" @?= Right "ཀ྄་ཨོཾ"
            , testCase "a final behind a vowel stays silent" $
                rendered "oM" @?= Right []
            , testCase "a caret with no stack in front of it stands as written" $
                converted "^ra" @?= Right "^ར"
            , testCase "a caret with nothing before it is blamed" $
                rendered "^ra"
                    @?= Right ["line 1: \"^\": The final \"^\" closes no letter."]
            , testCase "the stray mark carries its code and severity" $
                recordsOf "Mi" @?= Right [(LeadingFinal, SevWarning)]
            , testCase "a Tibetan stray mark stands as it was written" $
                tibetanConverted "ཾ" @?= Right "ཾ"
            ]
        , testGroup
            "a stack dot that joins nothing"
            [ testCase "the dot in the tail is blamed" $
                rendered "ka."
                    @?= Right ["line 1: \"ka.\": The stack dot \".\" joins no stack to a letter."]
            , testCase "the dot stays as it was written" $
                converted "ka." @?= Right "ཀ."
            , testCase "one finding for the run, however many dots it holds" $
                rendered "ka.."
                    @?= Right ["line 1: \"ka..\": The stack dot \".\" joins no stack to a letter."]
            , testCase "a dot the grammar used is no orphan" $
                rendered "g.yag" @?= Right []
            , testCase "in one run, only the dot the grammar left is blamed" $
                rendered "g.yag."
                    @?= Right ["line 1: \"g.yag.\": The stack dot \".\" joins no stack to a letter."]
            , testCase "a plus the grammar used is no orphan" $
                rendered "sat+t+wa ba" @?= Right []
            , testCase "the dot is blamed after the window findings of its run" $
                rendered "g....yag"
                    @?= Right
                        [ "line 1: \"g....yag\": The prefix \"g\" does not allow \".\" after it."
                        , "line 1: \"g....yag\": The stack dot \".\" joins no stack to a letter."
                        ]
            , testCase "the orphan dot carries its code and severity" $
                recordsOf "ka." @?= Right [(OrphanDot, SevWarning)]
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

-- | What a Wylie input becomes. A rule that changes how the input is read has
-- to be checked on the text the converter hands back as well as on the message,
-- since the two can disagree: the message names the problem, the text shows
-- what was made of the input instead.
converted :: Text -> Either Text Text
converted = spellingToUnicode Wylie

-- | The same for a Tibetan-script input.
tibetanConverted :: Text -> Either Text Text
tibetanConverted = spellingToUnicode Tibetan

spellingToUnicode :: Spelling -> Text -> Either Text Text
spellingToUnicode spelling input = do
    items <- parseEither (pSentence spelling) (fst (tokenize input))
    pure (renderItems OutUnicode items)
    where
        tokenize = case spelling of
            Wylie -> tokenizeWylie
            Tibetan -> tokenizeUnicode

-- | Every preferred spelling a Wylie input is recommended, so that a form
-- which must stay quiet is checked by what it recommends and not by the whole
-- channel: the word may still be blamed by another rule, and the gate the
-- recommendation carries ('noteAmbiguous') is there precisely to keep the two
-- apart.
recommendations :: Text -> Either Text [Text]
recommendations = recommended Wylie

-- | The same for a Tibetan-script input. The rule speaks in Wylie, and the
-- Tibetan spelling writes no implicit vowel, so the same syllable read the
-- other way round is left alone.
tibetanRecommendations :: Text -> Either Text [Text]
tibetanRecommendations = recommended Tibetan

recommended :: Spelling -> Text -> Either Text [Text]
recommended spelling input = do
    items <- parseEither (pSentence spelling) (fst (tokenize input))
    pure
        [ prefers d
        | d <- diagnosticList (legality items)
        , diagCode d == AmbiguousSpelling
        ]
    where
        tokenize = case spelling of
            Wylie -> tokenizeWylie
            Tibetan -> tokenizeUnicode

-- | The spelling one recommendation names: our own message with the prefix and
-- the closing quote and period taken off, so a form reads as the word the rule
-- quotes and nothing else.
prefers :: Diagnostic -> Text
prefers d =
    case T.stripPrefix messagePrefix (diagMessage d) of
        Just quoted -> T.dropWhileEnd (== '"') (T.dropEnd 1 quoted)
        Nothing -> "unreadable recommendation"
    where
        messagePrefix :: Text
        messagePrefix = "The syllable is ambiguous; the preferred spelling is \""

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
