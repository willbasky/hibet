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
import Label (LabelFull (..), Labels (..), Title (..))
import Parse (Script (Script), ScriptType (..), WylieTibetMap, splitSyllables)
import Paths (dictPath1, dictPath2, sylPath, titlePath)
import Type (HibetError (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    mockMakeEnvSpec
    syllables
    translate

mockMakeEnvSpec :: Spec
mockMakeEnvSpec =
    describe "FileIO: " $ do
        it "Read file syllables" $ do
            res <- runFileMock (EF.readFileBS sylPath)
            res `shouldBe` Right syllablesRaw
        it "Read file titles" $ do
            res <- runFileMock (EF.readFileBS titlePath)
            res `shouldBe` Right toml
        it "Read file lazy dict 1" $ do
            res <- runFileMock (EF.readFileLazyBS dictPath1)
            res `shouldBe` Right dict1
        it "Read file lazy dict 2" $ do
            res <- runFileMock (EF.readFileLazyBS dictPath2)
            res `shouldBe` Right dict2

syllables :: Spec
syllables =
    describe "Syllables" $ do
        it "Split syllables" $ do
            case splitSyllables $ TE.decodeUtf8 syllablesRaw of
                Left err -> expectationFailure $ show err
                Right res -> sort res `shouldBe` sort wylieTibetSyl

translate :: Spec
translate =
    describe "Translate Wylie request" $ do
        it "Search mir" $ do
            res <- runFileMock makeEnv
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let query = "mir"
                    let reply = mapMaybe (searchTranslation query) env.dictionaryMeta
                    reply
                        `shouldBe` [ Answer
                                        { targets = [Target "as/for/ to a human being"]
                                        , dictNumber = 16
                                        , dictTitle = Title "Ives Waldo"
                                        }
                                   , Answer
                                        { targets = [Target "termin. case of mi"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
                                   , Answer
                                        { targets =
                                            [ Target
                                                "mi ru zhes pa'i ru yig bsdus pa ste/ srin po mir brdzus zhes pa lta bu/ (mir chags) ma'i mngal gyi byis pa/"
                                            ]
                                        , dictNumber = 37
                                        , dictTitle = Title "dag-yig gsar-bsgrigs"
                                        }
                                   ]
        it "Search ri ma" $ do
            res <- runFileMock makeEnv
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
            res <- runFileMock makeEnv
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let reply = mapMaybe (searchTranslation "re ba byed pa") env.dictionaryMeta
                    reply
                        `shouldBe` [ Answer
                                        { targets = [Target "demand, ask, hope, wish, expect"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
                                   , Answer
                                        { targets = [Target "to hope, wish, expect, demand, ask"]
                                        , dictNumber = 7
                                        , dictTitle = Title "Rangjung Yeshe"
                                        }
                                   ]
        it "Search re ba med pa" $ do
            res <- runFileMock makeEnv
            case res of
                Left err -> expectationFailure $ show err
                Right env -> do
                    let reply = mapMaybe (searchTranslation "re ba med pa") env.dictionaryMeta
                    reply
                        `shouldBe` [ Answer
                                        { targets = [Target "hopeless; no hope"]
                                        , dictNumber = 5
                                        , dictTitle = Title "Hopkins"
                                        }
                                   , Answer
                                        { targets = [Target "{MSA}nirapekṣa; {MSA}niṣpratikāṅkṣa"]
                                        , dictNumber = 41
                                        , dictTitle = Title "Hopkins Sanskrit"
                                        }
                                   , Answer
                                        { targets = [Target "hopeless, no hope of --"]
                                        , dictNumber = 16
                                        , dictTitle = Title "Ives Waldo"
                                        }
                                   , Answer
                                        { targets = [Target "hopeless, despairing"]
                                        , dictNumber = 15
                                        , dictTitle = Title "James Valby"
                                        }
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

-- Data for mocking

syllablesRaw :: BS.ByteString
syllablesRaw =
    "bla|\224\189\150\224\190\179\nbla'am|\224\189\150\224\190\179\224\189\160\224\189\152\nblab|\224\189\150\224\190\179\224\189\150\nblabs|\224\189\150\224\190\179\224\189\150\224\189\166\nblad|\224\189\150\224\190\179\224\189\145\nblag|\224\189\150\224\190\179\224\189\130\nblags|\224\189\150\224\190\179\224\189\130\224\189\166\nbla'i|\224\189\150\224\190\179\224\189\160\224\189\178\nman|\224\189\152\224\189\147\nmaN|\224\189\152\224\189\142\nmAn|\224\189\152\224\189\177\224\189\147\nmAN|\224\189\152\224\189\177\224\189\142\nmana|\224\189\152\224\189\147\nmAna|\224\189\152\224\189\177\224\189\147\nmANa|\224\189\152\224\189\177\224\189\142\nre|\224\189\162\224\189\186\nme|\224\189\152\224\189\186\n"

toml :: BS.ByteString
toml =
    "[[titles]]\n    path = \"Hopkins-2015-T|E\"\n    id = 5\n    label = \"Hopkins\"\n    about = \"The Uma Institute for Tibetan Studies Tibetan-Sanskrit-English Dictionary (Version: June 2015)|Jeffrey Hopkins, Editor.|Paul Hackett, Contributor and Technical Editor.| Contributors: Nathaniel Garson, William Magee, Andres Montano, John Powers, Craig Preston, Joe Wilson, Jongbok Yi|A PDF version of this dictionary is available for download at: www.uma-tibet.org\"\n    abbreviations = \"Hopkins\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n    year = 2015\n\n[[titles]]\n    path = \"RangjungYeshe-T|E\"\n    id = 7\n    label = \"Rangjung Yeshe\"\n    about = \"Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org\"\n    abbreviations = \"RangjungYeshe\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n"

dict1 :: BSL.ByteString
dict1 = "re|each; every; single; hope\nre ba|hope\nre ba med pa|hopeless; no hope\n"

dict2 :: BSL.ByteString
dict2 =
    "re ba|1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair\nre ba byed pa|to hope, wish, expect, demand, ask\nre ba chad|give up hope\nme|fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]\nre|noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]\n"

wylieTibetHM :: WylieTibetMap
wylieTibetHM =
    HM.fromList
        [ (Script "bla", Script "\3926\4019")
        , (Script "re", Script "\3938\3962")
        , (Script "mAn", Script "\3928\3953\3923")
        , (Script "maN", Script "\3928\3918")
        , (Script "mAN", Script "\3928\3953\3918")
        , (Script "man", Script "\3928\3923")
        , (Script "me", Script "\3928\3962")
        , (Script "blad", Script "\3926\4019\3921")
        , (Script "blabs", Script "\3926\4019\3926\3942")
        , (Script "blab", Script "\3926\4019\3926")
        , (Script "bla'i", Script "\3926\4019\3936\3954")
        , (Script "bla'am", Script "\3926\4019\3936\3928")
        , (Script "mana", Script "\3928\3923")
        , (Script "mAna", Script "\3928\3953\3923")
        , (Script "mANa", Script "\3928\3953\3918")
        , (Script "blags", Script "\3926\4019\3906\3942")
        , (Script "blag", Script "\3926\4019\3906")
        ]

wylieTibetSyl :: [(Script 'Wylie, Script 'Tibet)]
wylieTibetSyl =
    [ (Script "bla", Script "\3926\4019")
    , (Script "re", Script "\3938\3962")
    , (Script "mAn", Script "\3928\3953\3923")
    , (Script "maN", Script "\3928\3918")
    , (Script "mAN", Script "\3928\3953\3918")
    , (Script "man", Script "\3928\3923")
    , (Script "me", Script "\3928\3962")
    , (Script "blad", Script "\3926\4019\3921")
    , (Script "blabs", Script "\3926\4019\3926\3942")
    , (Script "blab", Script "\3926\4019\3926")
    , (Script "bla'i", Script "\3926\4019\3936\3954")
    , (Script "bla'am", Script "\3926\4019\3936\3928")
    , (Script "mana", Script "\3928\3923")
    , (Script "mAna", Script "\3928\3953\3923")
    , (Script "mANa", Script "\3928\3953\3918")
    , (Script "blags", Script "\3926\4019\3906\3942")
    , (Script "blag", Script "\3926\4019\3906")
    ]
