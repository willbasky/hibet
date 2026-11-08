module App
    ( app
    )
where

import Cli (Command (Shell), parser, runCommand)
import Env (Env, makeEnvFor)
import Type (HibetError (..))

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Ki
import Options.Applicative (execParser)
import System.IO (hFlush, stdout)

app :: IO ()
app = handleErrors $ do
    com <- execParser parser
    let selected = case com of
            Shell ids -> ids
            _ -> []
    mv <- newEmptyMVar
    Ki.scoped $ \scope -> do
        _ <- Ki.fork scope (prepareEnv selected mv)
        runCommand mv com

prepareEnv :: [Int] -> MVar Env -> IO ()
prepareEnv selected mv =
    makeEnvFor selected >>= either throwIO (putMVar mv)

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
