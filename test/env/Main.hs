{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main where

import Dictionary
import Effects.File (FileIO (..), HibetErrors (..))
import qualified Effects.File as EF
import Env (Env (..), makeEnv)
import Label
import Paths

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.HashMap.Strict (fromList)
import Data.Text (pack)
import Path (mkAbsDir, mkAbsFile)
import Polysemy (Member, Sem)
import qualified Polysemy as P
import Polysemy.Error (Error, runError, throw)
import Polysemy.Path (Abs, Dir, File)
import System.Exit
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Debug.Trace
import System.Directory


main :: IO ()
main = hspec $ do
  mockMakeEnvSpec

mockMakeEnvSpec :: Spec
mockMakeEnvSpec =
  describe "Make env. " $ do
    it "GetPath syllabies" $ do
      case runFileMock (EF.getPath "stuff/tibetan-syllables") of
        Left err -> do
          print err
          exitFailure
        Right r -> r `shouldBe` sylPath
    it "GetPath titles" $ do
      case runFileMock (EF.getPath "stuff/titles.toml") of
        Left err -> do
          print err
          exitFailure
        Right r -> r `shouldBe` titlePath
    it "GetPath dicts" $ do
      case runFileMock (EF.getPath "dicts/") of
        Left err -> do
          print err
          exitFailure
        Right r -> r `shouldBe` dictDir

    it "Read file syllabies" $ do
      case runFileMock (EF.readFile sylPath) of
        Left err -> do
          print err
          exitFailure
        Right r -> r `shouldBe` syllabies
    it "Read file titles" $ do
      traceM titlePath
      case runFileMock (EF.readFile titlePath) of
        Left err -> do
          print err
          exitFailure
        Right r -> r `shouldBe` toml
    -- it "Read file lazy dict 1" $ do
    --   traceM =<< getCurrentDirectory
    --   (traceM . show) =<< doesFileExist dictPath1
    --   -- traceM =<< getHomeDirectory
    --   traceM dictPath1
    --   case runFileMock (EF.readFileLazy dictPath1) of
    --     Left err -> do
    --       print err
    --       exitFailure
    --     Right r -> r `shouldBe` dict1
    -- it "Read file lazy dict 2" $ do
    --   traceM dictPath2
    --   case runFileMock (EF.readFileLazy dictPath2) of
    --     Left err -> do
    --       print err
    --       exitFailure
    --     Right r -> r `shouldBe` dict2

    -- it "Success with Test 1" $ do
    --   case runFileMock makeEnv of
    --     Left err -> do
    --       print err
    --       exitFailure
    --     Right env -> env.dictionaryMeta `shouldBe` [meta1,meta2]


runFileMock :: Sem
  '[  FileIO
    , Error HibetErrors
    ] a
  -> (Either HibetErrors a)
runFileMock program = program
  & interpretFileMock
  & runError @HibetErrors
  & P.run


interpretFileMock :: Member (Error HibetErrors) r
  => Sem (FileIO : r) a
  -> Sem r a
interpretFileMock = P.interpret $ \case
  ReadFile "test/env/data/stuff/tibetan-syllables" -> pure syllabies
  ReadFile "test/env/data/stuff/titles.toml" -> pure toml
  ReadFile fp -> throw $ UnknownError $ "Unknown path for readfile: " <> pack fp

  ReadFileLazy path -> case path of
    "/home/metaxis/sources/Haskell/apps/hibet/test/env/data/dicts/Berzin-T|E.txt" -> pure dict1
    "/home/metaxis/sources/Haskell/apps/hibet/test/env/data/dicts/RichardBarron-T|E.txt" -> pure dict2
    p -> throw $ UnknownError $ "Unknown read lazy path: " <> pack p

  GetPath "stuff/tibetan-syllables" -> pure sylPath
  GetPath "stuff/titles.toml" -> pure titlePath
  GetPath "dicts/" -> pure dictDir
  GetPath p -> throw $ UnknownError $ "Unknown get path: " <> pack  p

  ListDirectory _ ->
    pure ([], [$(mkAbsFile dictPath1), $(mkAbsFile dictPath2)])
  -- ListDirectory p -> throw $ UnknownError $ "Unknown list path: " <> toText p

  ParseAbsDirectory _ -> pure $(mkAbsDir dictDirAbs)
  -- ParseAbsDirectory p -> throw $ UnknownError $ "Unknown parse path: " <>pack  p

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
  { dictionary = fromList [("bla", [Target "life", Target "Sublime"])]
  , title      = Title "123"
  , number     = 1
  }

meta2 :: DictionaryMeta
meta2 = DictionaryMeta
  { dictionary =
      fromList [("bla", [Target "spirit (of a deceased person)"])]
  , title      = Title "234"
  , number     = 2
  }
