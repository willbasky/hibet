module Main where

import Dictionary
import Effects.File (FileIO (..))
import qualified Effects.File as EF
import Env (makeEnv)
import Label (LabelFull (..), Labels (..), Title (..))
import Parse (TibetSyllable (..), WylieSyllable (..), WylieTibetMap, splitSyllables)
import Paths (dictDir, dictPath1, dictPath2, sylPath, titlePath)
import Type (HibetError (..))
import Utility (mkAbsolute, pack)

import Control.Monad.Except (runExcept)
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
import qualified Polysemy.Path as PP
import Polysemy.Trace (Trace, runTraceList)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)


main :: IO ()
main = hspec $ do
  mockMakeEnvSpec
  -- syllabi
  translate

mockMakeEnvSpec ::Spec
mockMakeEnvSpec =
  describe "FileIO: " $ do
    it "GetPath syllabies" $ do
      res <- snd <$> runFileMock (EF.getPath "stuff/tibetan-syllables")
      res `shouldBe` Right sylPath
    it "GetPath titles" $ do
      res <- snd <$> runFileMock (EF.getPath "stuff/titles.toml")
      res `shouldBe` Right titlePath
    it "GetPath dicts" $ do
      res <- snd <$> runFileMock (EF.getPath "dicts/")
      res `shouldBe` Right dictDir

    it "Read file syllabies" $ do
      res <- snd <$> runFileMock (EF.readFile sylPath)
      -- print res
      -- print sylPath
      res `shouldBe` Right syllables
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
        Right env -> env.wylieTibetMap `shouldBe` wylieTibetHM -- Todo: fix expected

syllabi :: Spec
syllabi =
  describe "Syllables" $ do
    it "Split syllables" $ do
      case runExcept $ splitSyllables $ TE.decodeUtf8 syllables of
        Left err  -> expectationFailure $ show err
        Right res -> res `shouldBe` wylieTibetSyl
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
          reply `shouldBe` [Answer {targets = [Target "noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]
    it "Search re ba" $ do
      res <- snd <$> runFileMock makeEnv
      case res of
        Left err  -> expectationFailure $ show err
        Right env -> do
          let reply = mapMaybe (searchTranslation "re ba") env.dictionaryMeta
          -- print rep.ly
          reply `shouldBe` [Answer {targets = [Target "1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair"], dictNumber = 7, dictTitle = Title "Rangjung Yeshe"}]


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
    f1' <- EF.mapErr $ PP.parseAbsFile $ mkAbsolute dictPath1
    f2' <- EF.mapErr $ PP.parseAbsFile $ mkAbsolute dictPath2
    pure ([], [f1', f2'])

  ParseAbsDir _ -> EF.mapErr $ PP.parseAbsDir $ mkAbsolute dictDir


-- Data for mocking


syllables :: BS.ByteString
syllables = "bla|\224\189\150\224\190\179\nbla'am|\224\189\150\224\190\179\224\189\160\224\189\152\nblab|\224\189\150\224\190\179\224\189\150\nblabs|\224\189\150\224\190\179\224\189\150\224\189\166\nblad|\224\189\150\224\190\179\224\189\145\nblag|\224\189\150\224\190\179\224\189\130\nblags|\224\189\150\224\190\179\224\189\130\224\189\166\nbla'i|\224\189\150\224\190\179\224\189\160\224\189\178\nman|\224\189\152\224\189\147\nmaN|\224\189\152\224\189\142\nmAn|\224\189\152\224\189\177\224\189\147\nmAN|\224\189\152\224\189\177\224\189\142\nmana|\224\189\152\224\189\147\nmAna|\224\189\152\224\189\177\224\189\147\nmANa|\224\189\152\224\189\177\224\189\142\nre|\224\189\162\224\189\186\nme|\224\189\152\224\189\186\n"

toml :: BS.ByteString
toml = "[[titles]]\n    path = \"Berzin-T|E\"\n    id = 8\n    label = \"Berzin\"\n    mergeLines = true\n    about = \"Dr. Alexander Berzin's English-Tibetan-Sanskrit Glossary|These entries are from the glossary of www.berzinarchives.com\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n\n[[titles]]\n    path = \"RangjungYeshe-T|E\"\n    id = 7\n    label = \"Rangjung Yeshe\"\n    about = \"Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org\"\n    abbreviations = \"RangjungYeshe\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n"

dict1 :: BSL.ByteString
dict1 = "bla|life spirit\nbla|Sublime\n"

dict2 :: BSL.ByteString
dict2 = "re ba|1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair\nre ba byed pa|to hope, wish, expect, demand, ask\nre ba chad|give up hope\nme|fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]\nre|noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]\n"

meta1 :: DictionaryMeta
meta1 = DictionaryMeta
  { dictionary = HM.fromList [("bla", [Target "Sublime", Target "life spirit"])]
  , title      = Title "Berzin"
  , number     = 8
  }

meta2 :: DictionaryMeta
meta2 = DictionaryMeta {dictionary = HM.fromList [("re ba",[Target "1) {re ba, re ba, re ba} intr. v. . to hope, aims; hopes, expectation. 2) woven cloth/ goat hair"]),("re",[Target "noun + re + noun - Das: Under \"re\", 4) Occurs as a particle mostly put between two closely connected words for the purpose of giving the compound word a verbal signification; thus {snying rje} signifying compassion, can be split in two with the particle {re} between them and then it means: to take pity upon {snying re rje}; in the same manner {'o brgyal} fatigue becomes {'o re brgyal} = was fatigued. In like manner, we have {nyams re dga'}; {blo re bde}, [to be delighted]; {skyug re log}; {zhe re 'jigs}; {yi re mug}; {don re chung}. MSS: To this list we can add: {zhe re skyid}; {dang re spro}; {sems re skyo}, I'm feeling sad, how sad. [mss]"]),("re ba byed pa",[Target "to hope, wish, expect, demand, ask"]),("me",[Target "fire, flame, ember, Anaka, [the 50th year, Male Fire Dragon]"]),("re ba chad",[Target "give up hope"])], title = Title "Rangjung Yeshe", number = 7}

labels :: Labels
labels = Labels
  {labelTitles = [LabelFull {path = "RangjungYeshe-T|E", lfId = 7, label = Title "Rangjung Yeshe", about = "Rangjung Yeshe Dictionary|Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 by Erik Pema Kunsang (2003)|online version: http://rywiki.tsadra.org", available = True, source = "Tibetan", target = Set.fromList ["English"], year = Nothing},LabelFull {path = "Berzin-T|E", lfId = 8, label = Title "Berzin", about = "Dr. Alexander Berzin's English-Tibetan-Sanskrit Glossary|These entries are from the glossary of www.berzinarchives.com", available = True, source = "Tibetan", target = Set.fromList ["English"], year = Nothing}]}

wylieTibetHM :: WylieTibetMap
wylieTibetHM = HM.fromList
  [ (WylieSyllable {unWylie = "bla"},TibetSyllable {unTibet = "\3926\4019"})
  , (WylieSyllable {unWylie = "re"},TibetSyllable {unTibet = "\3938\3962"})
  , (WylieSyllable {unWylie = "mAn"},TibetSyllable {unTibet = "\3928\3953\3923"})
  , (WylieSyllable {unWylie = "maN"},TibetSyllable {unTibet = "\3928\3918"})
  , (WylieSyllable {unWylie = "mAN"},TibetSyllable {unTibet = "\3928\3953\3918"})
  , (WylieSyllable {unWylie = "man"},TibetSyllable {unTibet = "\3928\3923"})
  , (WylieSyllable {unWylie = "me"},TibetSyllable {unTibet = "\3928\3962"})
  , (WylieSyllable {unWylie = "blad"},TibetSyllable {unTibet = "\3926\4019\3921"})
  , (WylieSyllable {unWylie = "blabs"},TibetSyllable {unTibet = "\3926\4019\3926\3942"})
  , (WylieSyllable {unWylie = "blab"},TibetSyllable {unTibet = "\3926\4019\3926"})
  , (WylieSyllable {unWylie = "bla'i"},TibetSyllable {unTibet = "\3926\4019\3936\3954"})
  , (WylieSyllable {unWylie = "bla'am"},TibetSyllable {unTibet = "\3926\4019\3936\3928"})
  , (WylieSyllable {unWylie = "mana"},TibetSyllable {unTibet = "\3928\3923"})
  , (WylieSyllable {unWylie = "mAna"},TibetSyllable {unTibet = "\3928\3953\3923"})
  , (WylieSyllable {unWylie = "mANa"},TibetSyllable {unTibet = "\3928\3953\3918"})
  , (WylieSyllable {unWylie = "blags"},TibetSyllable {unTibet = "\3926\4019\3906\3942"})
  , (WylieSyllable {unWylie = "blag"},TibetSyllable {unTibet = "\3926\4019\3906"})
  ]

wylieTibetSyl :: [(WylieSyllable,TibetSyllable)]
wylieTibetSyl =
  [ (WylieSyllable {unWylie = "bla"},TibetSyllable {unTibet = "\3926\4019"})
  , (WylieSyllable {unWylie = "re"},TibetSyllable {unTibet = "\3938\3962"})
  , (WylieSyllable {unWylie = "mAn"},TibetSyllable {unTibet = "\3928\3953\3923"})
  , (WylieSyllable {unWylie = "maN"},TibetSyllable {unTibet = "\3928\3918"})
  , (WylieSyllable {unWylie = "mAN"},TibetSyllable {unTibet = "\3928\3953\3918"})
  , (WylieSyllable {unWylie = "man"},TibetSyllable {unTibet = "\3928\3923"})
  , (WylieSyllable {unWylie = "me"},TibetSyllable {unTibet = "\3928\3962"})
  , (WylieSyllable {unWylie = "blad"},TibetSyllable {unTibet = "\3926\4019\3921"})
  , (WylieSyllable {unWylie = "blabs"},TibetSyllable {unTibet = "\3926\4019\3926\3942"})
  , (WylieSyllable {unWylie = "blab"},TibetSyllable {unTibet = "\3926\4019\3926"})
  , (WylieSyllable {unWylie = "bla'i"},TibetSyllable {unTibet = "\3926\4019\3936\3954"})
  , (WylieSyllable {unWylie = "bla'am"},TibetSyllable {unTibet = "\3926\4019\3936\3928"})
  , (WylieSyllable {unWylie = "mana"},TibetSyllable {unTibet = "\3928\3923"})
  , (WylieSyllable {unWylie = "mAna"},TibetSyllable {unTibet = "\3928\3953\3923"})
  , (WylieSyllable {unWylie = "mANa"},TibetSyllable {unTibet = "\3928\3953\3918"})
  , (WylieSyllable {unWylie = "blags"},TibetSyllable {unTibet = "\3926\4019\3906\3942"})
  , (WylieSyllable {unWylie = "blag"},TibetSyllable {unTibet = "\3926\4019\3906"})
  ]
