module App
    ( app
    )
where

import Cli
    ( Action (Shell)
    , Command (..)
    , commandNeedsEnv
    , parser
    , runCommand
    , runStatic
    )
import Env (Env, makeEnvForDataDir)
import Type (HibetError (..))

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Ki
import Options.Applicative
    ( customExecParser
    , noBacktrack
    , prefs
    , showHelpOnEmpty
    , showHelpOnError
    )
import System.IO (hFlush, stdout)

app :: IO ()
app = handleErrors $ do
    com <-
        customExecParser
            (prefs $ showHelpOnEmpty <> showHelpOnError <> noBacktrack)
            parser
    if commandNeedsEnv com.action
        then do
            let selected = case com.action of
                    Shell ids -> ids
                    _ -> []
            mv <- newEmptyMVar
            Ki.scoped $ \scope -> do
                _ <- Ki.fork scope (prepareEnv com.dataDir selected mv)
                runCommand mv com.action
        else runStatic com

prepareEnv :: Maybe FilePath -> [Int] -> MVar Env -> IO ()
prepareEnv dataDir selected mv =
    makeEnvForDataDir dataDir selected >>= either throwIO (putMVar mv)

handleErrors :: IO a -> IO ()
handleErrors action =
    (void action `catch` reportHibet) `catch` reportIO
    where
        reportHibet :: HibetError -> IO ()
        reportHibet = report
        reportIO :: IOException -> IO ()
        reportIO e = report (EffectError "IO error" (T.pack (show e)))
        report :: HibetError -> IO ()
        report err = do
            TIO.putStrLn "Hibet application failed with exception:"
            print err
            hFlush stdout
