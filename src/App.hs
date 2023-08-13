{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (Env, makeEnv, putEnvMVar)
import Type (HibetError (..))
import Utility (debugEnabledEnvVar)
import Data.Text.IO as TIO
import Data.Function ((&))
import IncipitCore (Async, asyncToIOFinal, SomeException)
import System.IO

import Effectful
import Effectful.Error.Static
import Effectful.Concurrent
import Effectful.Reader.Dynamic
import Effectful.Concurrent.MVar.Strict
import Effectful.Reader.Dynamic
import Effectful.Resource
import Effectful.Log
import Control.Exception (finally)

-- import Polysemy (Embed, Final, Members, Sem, embedToFinal, runFinal)
-- import Polysemy.Conc (Race, Sync, interpretRace, interpretSync, withAsync_)
-- import Polysemy.Error (Error, runError)
-- import Polysemy.Resource (Resource, runResource)
-- import Polysemy.Trace (Trace, ignoreTrace, traceToStdout)


app :: IO ()
app = do
  print ""
  isDebug <- debugEnabledEnvVar
  handleHibetError =<< interpretHibet hibet isDebug

type HibetEffects es =
  (
    Resource :> es
  , Error HibetError :> es
  , Error SomeException :> es
  , FileSystem :> es
  , Console :> es
  , Reader (MVar Env) :> es
  , Concurrent :> es
  , PrettyPrint :> es
  , Log :> es
  -- , Race
  -- , Async
  -- , Embed IO
  -- , Final IO
  )

interpretHibet :: HibetEffects es => Eff es ()
  -> Bool -- isDebug
  -> IO (Either (CallStack, HibetError) ())
interpretHibet program isDebug = withStdOutLogger $ \logger ->
  program
    & runResource
    & runError @HibetError
    & runFileSystemIO
    & runConsole
    & runPrettyPrint
    & runConcurrent
    & runLog "hibet" logger defaultLogLevel
    $ runReader
    -- & (if isDebug then traceToStdout else ignoreTrace)
    -- & interpretSync @Env
    -- & interpretRace
    -- & asyncToIOFinal
    -- & embedToFinal
    & runEff

hibet :: HibetEffects es => Eff es ()
hibet = do
  -- withAsync_ prepareEnv $ do
    prepareEnv
    com <- execParser parser
    runCommand com

prepareEnv ::
  ( FileSystem :> es
  , Error HibetError :> es
  , Log :> es
  , Reader (MVar Env) :> es
  , IOE :> es
  ) => Eff es ()
prepareEnv = do
  !env <- makeEnv
  putEnvMVar env

handleHibetError :: Either HibetError a -> IO ()
handleHibetError = \case
  Right _ -> pure ()
  Left err -> do
    TIO.putStrLn "Hibet application failed with exception:"
    print err

withStdOutLogger :: (Logger -> IO r) -> IO r
withStdOutLogger act = do
  logger <- mkLogger "stdout" $ \msg -> do
    TIO.putStrLn $ showLogMessage Nothing msg
    hFlush stdout
  withLogger logger act

withLogger :: Logger -> (Logger -> IO r) -> IO r
withLogger logger act = act logger `finally` cleanup
  where
    cleanup = waitForLogger logger >> shutdownLogger logger
-- Prevent GHC from inlining this function so its callers are
{-# NOINLINE withLogger #-}
