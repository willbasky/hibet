module Main where

import Dictionary
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static
    ( Error
    , runErrorNoCallStack
    )
import Effects.File (FileSystem (..))
import qualified Effects.File as EF
import Env (Env (..), makeEnv)
import Label (Title (..))
import Paths (dictPath1, dictPath2, titlePath)
import Translator (getAnswer)
import Type (HibetError (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HMS
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    mockMakeEnvSpec
    dictionarySearchSpec
    translate

dictionarySearchSpec :: Spec
dictionarySearchSpec =
    describe "Dictionary search" $ do
        it "finds a key" $
            search "re ba" "re ba|hope\n"
                `shouldBe` Just (Answer [Target "hope"] 1 (Title "test"))
        it "returns Nothing for an absent key" $
            search "me" "re ba|hope\n" `shouldBe` Nothing
        it "keeps every meaning of a duplicated key" $
            fmap
                (sortOn unTarget . targets)
                (search "re ba" "re ba|hope\nre ba|expectation\n")
                `shouldBe` Just [Target "expectation", Target "hope"]
        it "drops an exact duplicate line" $
            fmap targets (search "re ba" "re ba|hope\nre ba|hope\n")
                `shouldBe` Just [Target "hope"]
        it "treats an empty target list as no answer" $
            searchTranslation "re ba" (meta (HMS.fromList [("re ba", [])]))
                `shouldBe` Nothing
    where
        search query raw = searchTranslation query (meta (makeDictionary raw))
        meta dictionary = DictionaryMeta dictionary (Title "test") 1

mockMakeEnvSpec :: Spec
mockMakeEnvSpec =
    describe "FileIO: " $ do
        it "Read file titles" $ do
            res <- runFileMock (EF.readFileBS titlePath)
            res `shouldBe` Right toml
        it "Read file lazy dict 1" $ do
            res <- runFileMock (EF.readFileLazyBS dictPath1)
            res `shouldBe` Right dict1
        it "Read file lazy dict 2" $ do
            res <- runFileMock (EF.readFileLazyBS dictPath2)
            res `shouldBe` Right dict2

translate :: Spec
translate =
    describe "Translate Wylie request" $ do
        it "Search mir" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let query = "mir"
                    let reply = mapMaybe (searchTranslation query) env.dictionaryMeta
                    sortOn dictNumber reply
                        `shouldBe` [ Answer
                                        { targets = [Target "termin. case of mi"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
                                   , Answer
                                        { targets = [Target "as/for/ to a human being"]
                                        , dictNumber = 16
                                        , dictTitle = Title "Ives Waldo"
                                        }
                                   , Answer
                                        { targets =
                                            [ Target
                                                "{mi ru zhes pa'i ru yig bsdus pa ste/ srin po mir brdzus zhes pa lta bu/ (mir chags) ma'i mngal gyi byis pa/}"
                                            ]
                                        , dictNumber = 37
                                        , dictTitle = Title "dag-yig gsar-bsgrigs"
                                        }
                                   ]
        it "Search ri ma" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let reply = mapMaybe (searchTranslation "ri ma") env.dictionaryMeta
                    reply
                        `shouldBe` [ Answer
                                        { targets = [Target "arid/ non-irrigated land"]
                                        , dictNumber = 16
                                        , dictTitle = Title "Ives Waldo"
                                        }
                                   ]
        it "Search re ba byed pa" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let reply = mapMaybe (searchTranslation "re ba byed pa") env.dictionaryMeta
                    sortOn dictNumber reply
                        `shouldBe` [ Answer
                                        { targets = [Target "to hope, wish, expect, demand, ask"]
                                        , dictNumber = 7
                                        , dictTitle = Title "Rangjung Yeshe"
                                        }
                                   , Answer
                                        { targets = [Target "demand, ask, hope, wish, expect"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
                                   ]
        it "Search re ba med pa" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let reply = mapMaybe (searchTranslation "re ba med pa") env.dictionaryMeta
                    sortOn dictNumber reply
                        `shouldBe` [ Answer
                                        { targets = [Target "hopeless; no hope"]
                                        , dictNumber = 5
                                        , dictTitle = Title "Hopkins"
                                        }
                                   , Answer
                                        { targets = [Target "hopeless, despairing"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
                                   , Answer
                                        { targets = [Target "hopeless, no hope of --"]
                                        , dictNumber = 16
                                        , dictTitle = Title "Ives Waldo"
                                        }
                                   , Answer
                                        { targets = [Target "[MSA]nirapekṣa; [MSA]niṣpratikāṅkṣa"]
                                        , dictNumber = 41
                                        , dictTitle = Title "Hopkins Sanskrit"
                                        }
                                   ]
        it "Tibetan query reaches the same entries as its Wylie key" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env ->
                    case (getAnswer "re ba med pa" env, getAnswer "རེ་བ་མེད་པ།" env) of
                        (Right (_, fromWylie), Right (_, fromTibet)) -> do
                            fromTibet `shouldBe` fromWylie
                            length fromTibet `shouldBe` 4
                        answers -> expectationFailure $ show answers
        it "Unicode query matrix reaches the same entries as its Wylie key" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                Right env ->
                    mapM_
                        (\(wylie, unicode) -> answersOf env unicode `shouldBe` answersOf env wylie)
                        unicodeMatrix
        it "A malformed syllable does not resolve to the clean phrase" $ do
            res <- runFileMock (withTestDicts <$> makeEnv)
            case res of
                Left err -> expectationFailure $ show err
                -- The broken run contributes no key, so the typo cannot pass
                -- for the phrase the user meant.
                Right env -> answersOf env "ྐརེ་བ་མེད་པ" `shouldBe` Right []

answersOf :: Env -> Text -> Either HibetError [Answer]
answersOf env query = snd <$> getAnswer query env

-- | Every Unicode input stands for the Wylie key next to it. The cases are
-- the ways a query differs from a dictionary key: punctuation, spacing and
-- line breaks, foreign text (bare and bracketed), a lookalike Devanagari danda
-- instead of a shad, and digits.
unicodeMatrix :: [(Text, Text)]
unicodeMatrix =
    [ ("re ba med pa", "རེ་བ་མེད་པ།")
    , ("re ba med pa", "རེ་བ་ མེད་པ །")
    , ("re ba med pa", "རེ་བ་\nམེད་པ།")
    , ("re ba med pa", "རེ་བ་ hello མེད་པ།")
    , ("re ba med pa", "རེ་བ་ [hello] མེད་པ།")
    , ("re ba med pa", "རེ་ བ་ མེད་ པ")
    , ("re ba med pa", "རེ་བ་མེད་པ।")
    , ("re ba med pa", "རེ་བ་མེད་པ་༢༠༢༤")
    , ("re ba byed pa", "རེ་བ་བྱེད་པ།")
    , ("re ba", "རེ་བ།")
    , ("ri ma", "རི་མ།")
    ]

runFileMock ::
    Eff
        '[ FileSystem
         , Error HibetError
         , -- , Log
           IOE
         ]
        a
    -> IO (Either HibetError a)
runFileMock program =
    program
        & EF.runFileSystemIO
        & runErrorNoCallStack @HibetError
        -- & runLog "hibet" logger defaultLogLevel
        & runEff

-- The expectations below are written for these dictionaries. New dictionaries
-- must not change the result of the search tests.
testDicts :: [Int]
testDicts = [5, 7, 15, 16, 37, 41]

withTestDicts :: Env -> Env
withTestDicts env = env{dictionaryMeta = selectDict testDicts env.dictionaryMeta}

-- Data for mocking

toml :: BS.ByteString
toml =
    "[[titles]]\n    path = \"Hopkins-2015-T|E\"\n    id = 5\n    label = \"Hopkins\"\n    about = \"The Uma Institute for Tibetan Studies Tibetan-Sanskrit-English Dictionary (Version: June 2015)|Jeffrey Hopkins, Editor.|Paul Hackett, Contributor and Technical Editor.| Contributors: Nathaniel Garson, William Magee, Andres Montano, John Powers, Craig Preston, Joe Wilson, Jongbok Yi|A PDF version of this dictionary is available for download at: www.uma-tibet.org\"\n    abbreviations = \"Hopkins\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n    year = 2015\n\n[[titles]]\n    path = \"RangjungYeshe-T|E\"\n    id = 7\n    label = \"Rangjung Yeshe\"\n    about = \"Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org\"\n    abbreviations = \"RangjungYeshe\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n"

dict1 :: BSL.ByteString
dict1 = "re|each; every; single; hope\nre ba|hope\nre ba med pa|hopeless; no hope\n"

dict2 :: BSL.ByteString
dict2 =
    "re ba|1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair\nre ba byed pa|to hope, wish, expect, demand, ask\nre ba chad|give up hope\nme|fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]\nre|noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]\n"
