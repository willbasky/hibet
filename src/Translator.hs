{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Translator
  ( translator
  )
  where

import Dictionary (selectDict, getAnswer)
-- import Effects.File (FileIO)
-- import qualified Effects.File as File
-- import Labels (getLabels)
-- import Parse
-- import qualified Polysemy as P
-- import Polysemy (Members, Sem)
-- import Polysemy.Error (Error, throw)
-- import Polysemy.Path (Abs, File, Path, PathException, fromAbsFile, parseAbsDir)
-- import Translator (loopDialog)
import Types
import Pretty

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (foldl')
import Data.Text (Text)
import System.Console.Haskeline (getHistory, getInputLine, defaultSettings)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO
import System.Exit (exitSuccess)

import qualified Text.Megaparsec.Error as ME

-- import Control.DeepSeq
import Control.Exception (bracketOnError)
-- import Control.Monad.Reader
-- import Control.Parallel.Strategies
-- import Debug.Trace
-- import Path (fromAbsFile, parseAbsDir)

-- import Path.IO (listDir)
-- import Paths_hibet (getDataFileName)

-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
-- import Data.Text (Text)
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TLE




-- | Load environment and start loop dialog
translator :: [Int] -> Hibet ()
translator selectedDicts = do
  withReaderT (\env -> env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}) $
    ReaderT $ \env -> do
      bracketOnError
        (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such as a SigINT is received.
        (\inputState -> runReaderT (loopDialog inputState) env >> closeInput inputState)
        -- (\inputState -> runReaderT (testDialog inputState) env >> closeInput inputState) -- for tests



-- Looped dialog with user
loopDialog :: InputState -> Hibet ()
loopDialog inputState = ReaderT $ \env -> forever $ do
    putColorDoc blue NewLine "Input a tibetan word in wylie transcription, please."
    mQuery <- queryInput inputState $ getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "Bye-bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> queryInput inputState getHistory
            mapM_ (putColorDoc id NewLine) history
        Just query -> do
            let answerE = runExcept $ getAnswer query env
            case answerE of
                Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
                Right (answer, isEmpty) ->
                    if isEmpty then putColorDoc red NewLine "Nothing found"
                    else pprint answer

-- testDialog :: InputState -> Hibet ()
-- testDialog inputState = ReaderT $ \env -> forever $ do
--   putColorDoc blue NewLine "Which a tibetan word to translate?"
--   let answerE = runExcept $ getAnswer "mo" env
--   case answerE of
--       Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
--       Right (answer, isEmpty) ->
--           if isEmpty then putColorDoc red NewLine "Nothing found"
--           else print answer
--   exitSuccess

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines



-- -- Make environment
-- makeEnv :: IO Env
-- makeEnv = undefined

-- makeEnvS :: Members
--   [ FileIO
--   , Error PathException
--   , Error Text] r => Sem r Env
-- makeEnvS = do
--     sylsPath <- File.getPath "stuff/tibetan-syllables"
--     syls <- TE.decodeUtf8 <$> File.readFile sylsPath
--     labels@(Labels ls) <- getLabels <$> (File.readFile =<< File.getPath "stuff/titles.toml")
--     (_, files) <- File.listDirectory =<< parseAbsDir =<< File.getPath "dicts/"
--     filesAndTexts <- getFilesTexts files
--     let dictsMeta = parMap (rparWith rdeepseq) (\(f,t) -> toDictionaryMeta ls f $ makeTextMap $ TL.toStrict t) filesAndTexts
--     pure $ runEval $ do
--       wt <- rparWith rdeepseq $ makeWylieTibet syls
--       tw <- rparWith rdeepseq $ makeTibetWylie syls
--       wr <- rparWith rdeepseq $ makeWylieRadexTree syls
--       tr <- rparWith rdeepseq $ makeTibetanRadexTree syls
--       pure Env
--               { dictionaryMeta = dictsMeta
--               , wylieTibet = wt
--               , tibetWylie = tw
--               , radixWylie = wr
--               , radixTibet = tr
--               , labels     = labels
--               }

-- getFilesTexts :: Members
--   [ FileIO
--   , Error Text] r
--   => [Path Abs File] -> Sem r [(FilePath, TL.Text)]
-- getFilesTexts fp = do
--   let paths = map fromAbsFile fp
--   let contents = map (fmap TLE.decodeUtf8 . File.readFileLazy) paths
--   txts <- sequenceA contents
--   if length paths == length txts
--     then pure $ zip paths txts
--     else throw @Text
--      "Not all dictionary files was read successfully. The Path and opened files have a divergence"

    -- getFilesTextsPar fs = mapM (\f -> do
    --   let path = fromAbsFile f
    --   txt <- TE.decodeUtf8 <$> BS.readFile path
    --   pure (path, txt)) fs

-- parMapC :: (a -> b) -> [a] -> Eval [b]
-- parMapC f [] = return []
-- parMapC f (a:as) = do
--    b <- rpar (f a)
--    bs <- parMapC f as
--    return (b:bs)

-- parMap strat f = withStrategy (parList strat) . map f

-- parTraverseC :: Applicative t1 => Strategy [b] -> (a -> t1 b) -> [a] -> t1 [b]
-- parTraverseC strat f = withStrategy (parTraversable strat) . traverse f
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
