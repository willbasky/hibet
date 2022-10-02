module Main where

import Dictionary
import Effects.File (FileIO (..))
import qualified Effects.File as EF
import Env (makeEnv)
import Label (LabelFull (..), Labels (..), Title (..))
import Parse (ScriptType (..), Script(Script), WylieTibetMap, splitSyllables)
import Paths (dictDir, dictPath1, dictPath2, sylPath, titlePath)
import Type (HibetError (..))
import Utility (mkAbsolute, pack)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import Path.IO (listDir)
import Polysemy (Embed, Members, Sem)
import qualified Polysemy as P
import Polysemy.Error (Error, runError, throw)
import Polysemy.Trace (Trace, runTraceList)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)

-- import System.Directory
-- import System.IO
-- import qualified Debug.Trace as Trace

main :: IO ()
main = hspec $ do
  mockMakeEnvSpec
  syllables
  translate

mockMakeEnvSpec ::Spec
mockMakeEnvSpec =
  describe "FileIO: " $ do
    it "GetPath syllables" $ do
      res <- snd <$> runFileMock (EF.getPath "stuff/tibetan-syllables")
      res `shouldBe` Right sylPath
    it "GetPath titles" $ do
      res <- snd <$> runFileMock (EF.getPath "stuff/titles.toml")
      res `shouldBe` Right titlePath
    it "GetPath dicts" $ do
      res <- snd <$> runFileMock (EF.getPath "dicts/")
      res `shouldBe` Right dictDir

    it "Read file syllables" $ do
      res <- snd <$> runFileMock (EF.readFile sylPath)
      res `shouldBe` Right syllablesRaw
    it "Read file titles" $ do
      res <- snd <$> runFileMock (EF.readFile titlePath)
      res `shouldBe` Right toml
    it "Read file lazy dict 1" $ do
      res <- snd <$> runFileMock (EF.readFileLazy dictPath1)
      res `shouldBe` Right dict1
    it "Read file lazy dict 2" $ do
      res <- snd <$> runFileMock (EF.readFileLazy dictPath2)
      res `shouldBe` Right dict2

    it "Make env. Meta" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> env.dictionaryMeta `shouldBe` [meta1,meta2]
    it "Make env. Labels" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> env.labels `shouldBe` labels
    it "Make env. WylieTibetMap" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> env.wylieTibetMap `shouldBe` wylieTibetHM

syllables :: Spec
syllables =
  describe "Syllables" $ do
    it "Split syllables" $ do
      case splitSyllables $ TE.decodeUtf8 syllablesRaw of
        Left err  -> expectationFailure $ show err
        Right res -> sort res `shouldBe` sort wylieTibetSyl
    it "Map and list of syllables" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> sort (HM.toList env.wylieTibetMap) `shouldBe` sort wylieTibetSyl

translate :: Spec
translate =
  describe "Translate Wylie request" $ do
    it "Search me" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let query = "me"
          let reply = mapMaybe (searchTranslation query) env.dictionaryMeta
          reply `shouldBe` [Answer {targets = [Target "fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]
    it "Search re" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let reply = mapMaybe (searchTranslation "re") env.dictionaryMeta
          -- print rep.ly
          reply `shouldBe` [Answer {targets = [Target "each; every; single; hope"], dictNumber = 5, dictTitle = Title "Hopkins"}, Answer {targets = [Target "noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]
    it "Search re ba" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let reply = mapMaybe (searchTranslation "re ba") env.dictionaryMeta
          -- print rep.ly
          reply `shouldBe` [Answer {targets = [Target "hope"], dictNumber = 5, dictTitle = Title "Hopkins"},Answer {targets = [Target "1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]
    it "Search re ba byed pa" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let reply = mapMaybe (searchTranslation "re ba byed pa") env.dictionaryMeta
          -- print rep.ly
          reply `shouldBe` [Answer {targets = [Target "to hope, wish, expect, demand, ask"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]
    it "Search re ba med pa" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let reply = mapMaybe (searchTranslation "re ba med pa") env.dictionaryMeta
          -- print rep.ly
          reply `shouldBe` [Answer {targets = [Target "hopeless; no hope"], dictNumber = 5, dictTitle = Title "Hopkins"}]

-- convert :: Spec
-- convert =
--   describe "Translate Wylie request" $ do
--     it "Search me" $ do

runFileMock :: Sem
  '[  FileIO
    , Error HibetError
    , Trace
    , Embed IO
    ] a
  -> IO ([String], Either HibetError a)
runFileMock program = program
  & interpretFileMock
  & runError @HibetError
  & runTraceList
  & P.runM

interpretFileMock :: Members [Embed IO, Error HibetError] r => Sem (FileIO : r) a -> Sem r a
interpretFileMock = P.interpret $ \case
  ReadFile path -> P.embed $ BS.readFile path
  ReadFileLazy path -> P.embed $ BSL.readFile path

  GetPath path -> case path of
    "stuff/tibetan-syllables" -> pure sylPath
    "stuff/titles.toml"       -> pure titlePath
    "dicts/"                  -> pure dictDir
    p                         -> throw $ UnknownError $ "Unknown get path: " <> pack  p

  ListDirectory _ -> do
    f1' <- EF.parseAbsFileS $ mkAbsolute dictPath1
    f2' <- EF.parseAbsFileS $ mkAbsolute dictPath2
    pure ([], [f1', f2'])

  ParseAbsDir _ -> EF.parseAbsDirS $ mkAbsolute dictDir


-- Data for mocking


syllablesRaw :: BS.ByteString
syllablesRaw = "bla|\224\189\150\224\190\179\nbla'am|\224\189\150\224\190\179\224\189\160\224\189\152\nblab|\224\189\150\224\190\179\224\189\150\nblabs|\224\189\150\224\190\179\224\189\150\224\189\166\nblad|\224\189\150\224\190\179\224\189\145\nblag|\224\189\150\224\190\179\224\189\130\nblags|\224\189\150\224\190\179\224\189\130\224\189\166\nbla'i|\224\189\150\224\190\179\224\189\160\224\189\178\nman|\224\189\152\224\189\147\nmaN|\224\189\152\224\189\142\nmAn|\224\189\152\224\189\177\224\189\147\nmAN|\224\189\152\224\189\177\224\189\142\nmana|\224\189\152\224\189\147\nmAna|\224\189\152\224\189\177\224\189\147\nmANa|\224\189\152\224\189\177\224\189\142\nre|\224\189\162\224\189\186\nme|\224\189\152\224\189\186\n"

toml :: BS.ByteString
toml = "[[titles]]\n    path = \"Hopkins-2015-T|E\"\n    id = 5\n    label = \"Hopkins\"\n    about = \"The Uma Institute for Tibetan Studies Tibetan-Sanskrit-English Dictionary (Version: June 2015)|Jeffrey Hopkins, Editor.|Paul Hackett, Contributor and Technical Editor.| Contributors: Nathaniel Garson, William Magee, Andres Montano, John Powers, Craig Preston, Joe Wilson, Jongbok Yi|A PDF version of this dictionary is available for download at: www.uma-tibet.org\"\n    abbreviations = \"Hopkins\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n    year = 2015\n\n[[titles]]\n    path = \"RangjungYeshe-T|E\"\n    id = 7\n    label = \"Rangjung Yeshe\"\n    about = \"Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org\"\n    abbreviations = \"RangjungYeshe\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n"

dict1 :: BSL.ByteString
dict1 = "re|each; every; single; hope\nre ba|hope\nre ba med pa|hopeless; no hope\n"

dict2 :: BSL.ByteString
dict2 = "re ba|1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair\nre ba byed pa|to hope, wish, expect, demand, ask\nre ba chad|give up hope\nme|fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]\nre|noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]\n"

meta1 :: DictionaryMeta
meta1 = DictionaryMeta {dictionary = HM.fromList [("re ba",[Target "hope"]),("re ba med pa",[Target "hopeless; no hope"]),("re",[Target "each; every; single; hope"])], title = Title "Hopkins", number = 5}

meta2 :: DictionaryMeta
meta2 = DictionaryMeta {dictionary = HM.fromList [("re ba",[Target "1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair"]),("re",[Target "noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]"]),("re ba byed pa",[Target "to hope, wish, expect, demand, ask"]),("me",[Target "fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]"]),("re ba chad",[Target "give up hope"])], title = Title "Rangjung Yeshe", number = 7}

labels :: Labels
labels = Labels
  {labelTitles = [LabelFull {path = "Hopkins-2015-T|E", lfId = 5, label = Title "Hopkins", about = "The Uma Institute for Tibetan Studies Tibetan-Sanskrit-English Dictionary (Version: June 2015)|Jeffrey Hopkins, Editor.|Paul Hackett, Contributor and Technical Editor.| Contributors: Nathaniel Garson, William Magee, Andres Montano, John Powers, Craig Preston, Joe Wilson, Jongbok Yi|A PDF version of this dictionary is available for download at: www.uma-tibet.org", available = True, source = "Tibetan", target = Set.fromList ["English"], year = Just 2015},LabelFull {path = "RangjungYeshe-T|E", lfId = 7, label = Title "Rangjung Yeshe", about = "Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org", available = True, source = "Tibetan", target = Set.fromList ["English"], year = Nothing}]}

wylieTibetHM :: WylieTibetMap
wylieTibetHM = HM.fromList
  [ (Script "bla",Script "\3926\4019")
  , (Script "re",Script "\3938\3962")
  , (Script "mAn",Script "\3928\3953\3923")
  , (Script "maN",Script "\3928\3918")
  , (Script "mAN",Script "\3928\3953\3918")
  , (Script "man",Script "\3928\3923")
  , (Script "me",Script "\3928\3962")
  , (Script "blad",Script "\3926\4019\3921")
  , (Script "blabs",Script "\3926\4019\3926\3942")
  , (Script "blab",Script "\3926\4019\3926")
  , (Script "bla'i",Script "\3926\4019\3936\3954")
  , (Script "bla'am",Script "\3926\4019\3936\3928")
  , (Script "mana",Script "\3928\3923")
  , (Script "mAna",Script "\3928\3953\3923")
  , (Script "mANa",Script "\3928\3953\3918")
  , (Script "blags",Script "\3926\4019\3906\3942")
  , (Script "blag",Script "\3926\4019\3906")
  ]

wylieTibetSyl :: [(Script 'Wylie, Script 'Tibet)]
wylieTibetSyl =
  [ (Script "bla",Script "\3926\4019")
  , (Script "re",Script "\3938\3962")
  , (Script "mAn",Script "\3928\3953\3923")
  , (Script "maN",Script "\3928\3918")
  , (Script "mAN",Script "\3928\3953\3918")
  , (Script "man",Script "\3928\3923")
  , (Script "me",Script "\3928\3962")
  , (Script "blad",Script "\3926\4019\3921")
  , (Script "blabs",Script "\3926\4019\3926\3942")
  , (Script "blab",Script "\3926\4019\3926")
  , (Script "bla'i",Script "\3926\4019\3936\3954")
  , (Script "bla'am",Script "\3926\4019\3936\3928")
  , (Script "mana",Script "\3928\3923")
  , (Script "mAna",Script "\3928\3953\3923")
  , (Script "mANa",Script "\3928\3953\3918")
  , (Script "blags",Script "\3926\4019\3906\3942")
  , (Script "blag",Script "\3926\4019\3906")
  ]
