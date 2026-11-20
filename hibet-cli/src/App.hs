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
import Type (HibetError, renderError)

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception
    ( Handler (..)
    , IOException
    , catches
    , displayException
    , throwIO
    )
import Control.Monad (void)
import Data.Text (Text)
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
import System.Exit (exitFailure)
import System.IO (stderr)

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
                runCommand com.dataDir mv com.action
        else runStatic com

prepareEnv :: Maybe FilePath -> [Int] -> MVar Env -> IO ()
prepareEnv dataDir selected mv =
    makeEnvForDataDir dataDir selected >>= either throwIO (putMVar mv)

handleErrors :: IO a -> IO ()
handleErrors action =
    void action `catches` [Handler reportHibet, Handler reportIO]
    where
        reportHibet :: HibetError -> IO ()
        reportHibet = report . renderError
        reportIO :: IOException -> IO ()
        reportIO = report . T.pack . displayException
        report :: Text -> IO ()
        report text = TIO.hPutStrLn stderr text >> exitFailure
