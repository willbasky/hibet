{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Dictionary
import Effects.File (FileIO (..))
import qualified Effects.File as EF
import Env (makeEnv)
import Label (LabelFull (..), Labels (..), Title (..))
import Parse (BimapWylieTibet, TibetSyllable(..), WylieSyllable(..))
import Paths (dictDir, dictPath1, dictPath2, sylPath, titlePath)
import Utility (filename, mkAbsolute, pack)
import Type (HibetError (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.Set as Set
import Polysemy (Member, Sem)
import qualified Polysemy as P
import Polysemy.Error (Error, runError, throw)
import qualified Polysemy.Path as PP
import Polysemy.Trace (Trace, runTraceList)
import Test.Hspec (Spec, describe, expectationFailure, hspec, it, shouldBe)
import qualified Data.Bimap as Bi

main :: IO ()
main = hspec $ do
  mockMakeEnvSpec

mockMakeEnvSpec ::Spec
mockMakeEnvSpec =
  describe "FileIO: " $ do
    it "GetPath syllabies" $ do
      let res = snd $ runFileMock (EF.getPath "stuff/tibetan-syllables")
      res `shouldBe` Right sylPath
    it "GetPath titles" $ do
      let res = snd $ runFileMock (EF.getPath "stuff/titles.toml")
      res `shouldBe` Right titlePath
    it "GetPath dicts" $ do
      let res = snd $ runFileMock (EF.getPath "dicts/")
      res `shouldBe` Right dictDir

    it "Read file syllabies" $ do
      let res = snd $ runFileMock (EF.readFile sylPath)
      res `shouldBe` Right syllabies
    it "Read file titles" $ do
      let res = snd $ runFileMock (EF.readFile titlePath)
      res `shouldBe` Right toml
    it "Read file lazy dict 1" $ do
      let res = snd $ runFileMock (EF.readFileLazy dictPath1)
      res `shouldBe` Right dict1
    it "Read file lazy dict 2" $ do
      let res = snd $ runFileMock (EF.readFileLazy dictPath2)
      res `shouldBe` Right dict2

    it "Make env. Meta" $ do
      case snd $ runFileMock makeEnv of
        Left err  -> expectationFailure $ show err
        Right env -> env.dictionaryMeta `shouldBe` [meta1,meta2]
    it "Make env. Labels" $ do
      case snd $ runFileMock makeEnv of
        Left err  -> expectationFailure $ show err
        Right env -> env.labels `shouldBe` labels
    it "Make env. BimapWylieTibet" $ do
      case snd $ runFileMock makeEnv of
        Left err  -> expectationFailure $ show err
        Right env -> env.bimapWylieTibet `shouldBe` biWylieTibet

runFileMock :: Sem
  '[  FileIO
    , Error HibetError
    , Trace
    ] a
  -> ([String], Either HibetError a)
runFileMock program = program
  & interpretFileMock
  & runError @HibetError
  & runTraceList
  & P.run

interpretFileMock :: Member (Error HibetError) r => Sem (FileIO : r) a -> Sem r a
interpretFileMock = P.interpret $ \case
  ReadFile path -> case path of
    "test/env/data/stuff/tibetan-syllables" -> pure syllabies
    "test/env/data/stuff/titles.toml" -> pure toml
    fp -> throw $ UnknownError $ "Unknown path for readfile: " <> pack fp

  ReadFileLazy path
    | filename path == "Berzin-T|E.txt" -> pure dict1
    | filename path == "RichardBarron-T|E.txt" -> pure dict2
    | otherwise -> throw $ UnknownError "Unknown read lazy path"

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


syllabies :: BS.ByteString
syllabies = "bla|\224\189\150\224\190\179\nbla'am|\224\189\150\224\190\179\224\189\160\224\189\152\nblab|\224\189\150\224\190\179\224\189\150\nblabs|\224\189\150\224\190\179\224\189\150\224\189\166\nblad|\224\189\150\224\190\179\224\189\145\nblag|\224\189\150\224\190\179\224\189\130\nblags|\224\189\150\224\190\179\224\189\130\224\189\166\nbla'i|\224\189\150\224\190\179\224\189\160\224\189\178\n"

toml :: BS.ByteString
toml = "[[titles]]\n    path = \"Berzin-T|E\"\n    id = 8\n    label = \"Berzin\"\n    mergeLines = true\n    about = \"Dr. Alexander Berzin's English-Tibetan-Sanskrit Glossary|These entries are from the glossary of www.berzinarchives.com\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n\n[[titles]]\n    path = \"RichardBarron-T|E\"\n    id = 18\n    label = \"Richard Barron\"\n    about = \"Richard Barron's glossary. \194\169 Copyright 2002 by Turquoise Dragon Media Services. Source: Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 (2003)|online version: http://rywiki.tsadra.org\"\n    public = true\n    listCredits = true\n    available = true\n    source = \"Tibetan\"\n    target = [\"English\"]\n"

dict1 :: BSL.ByteString
dict1 = "bla|life spirit\nbla|Sublime\n"

dict2 :: BSL.ByteString
dict2 = "bla|spirit (of a deceased person)\n"

meta1 :: DictionaryMeta
meta1 = DictionaryMeta
  { dictionary = HM.fromList [("bla", [Target "Sublime", Target "life spirit"])]
  , title      = Title "Berzin"
  , number     = 8
  }

meta2 :: DictionaryMeta
meta2 = DictionaryMeta
  { dictionary =
      HM.fromList [("bla", [Target "spirit (of a deceased person)"])]
  , title      = Title "Richard Barron"
  , number     = 18
  }

labels :: Labels
labels = Labels
  {labelTitles =
    [ LabelFull
        { path = "Berzin-T|E"
        , lfId = 8
        , label = Title "Berzin"
        , about = "Dr. Alexander Berzin's English-Tibetan-Sanskrit Glossary|These entries are from the glossary of www.berzinarchives.com"
        , available = True
        , source = "Tibetan"
        , target = Set.fromList ["English"]
        , year = Nothing}
    , LabelFull
        { path = "RichardBarron-T|E"
        , lfId = 18
        , label = Title "Richard Barron"
        , about = "Richard Barron's glossary. \169 Copyright 2002 by Turquoise Dragon Media Services. Source: Rangjung Yeshe Tibetan-English Dharma Dictionary 3.0 (2003)|online version: http://rywiki.tsadra.org"
        , available = True
        , source = "Tibetan"
        , target = Set.fromList ["English"]
        , year = Nothing
      }
    ]
  }

biWylieTibet :: BimapWylieTibet
biWylieTibet = Bi.fromList
  [ (WylieSyllable "blag"  , TibetSyllable "\3926\4019\3906")
  , (WylieSyllable "blabs" , TibetSyllable "\3926\4019\3926\3942")
  , (WylieSyllable "bla'am", TibetSyllable "\3926\4019\3936\3928")
  , (WylieSyllable "bla"   , TibetSyllable "\3926\4019")
  , (WylieSyllable "blab"  , TibetSyllable "\3926\4019\3926")
  , (WylieSyllable "blad"  , TibetSyllable "\3926\4019\3921")
  , (WylieSyllable "blags" , TibetSyllable "\3926\4019\3906\3942")
  , (WylieSyllable "bla'i" , TibetSyllable "\3926\4019\3936\3954")
  ]
