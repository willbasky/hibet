{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module App
       ( app
       ) where

import Cli (parser, runCommand)
import Env (Env, makeEnv, putEnvMVar)
import Type (HibetError (..))
import Data.Text.IO as TIO
import Data.Function ((&))
import System.IO ( hFlush, stdout, putStrLn )

import Effects.Console ( Console, execParser, runConsole )
import Effects.File ( FileSystem, runFileSystemIO )
import Effects.PrettyPrint ( PrettyPrint, runPrettyPrint )
import Effectful ( runEff, type (:>), Eff, IOE )
import Effectful.Error.Static
    ( CallStack, prettyCallStack, Error, runError )
import Effectful.Concurrent ( runConcurrent, Concurrent )
import Effectful.Concurrent.Async (withAsync)
import Effectful.Concurrent.MVar.Strict ( MVar, newEmptyMVar )
import Effectful.Reader.Dynamic ( runReader, Reader )
import Effectful.Resource ( runResource, Resource )
import Effectful.Log
    ( defaultLogLevel,
      showLogMessage,
      shutdownLogger,
      waitForLogger,
      mkLogger,
      runLog,
      Logger,
      Log )
import Control.Exception (finally)



app :: IO ()
app = do
  errs <- withStdOutLogger $ \logger -> interpretHibet hibet logger
  handleHibetError errs

type HibetEffects =
   '[
      Concurrent
    , Resource
    , Console
    , FileSystem
    , PrettyPrint
    , Error HibetError
    , Log
    , IOE
    ]


interpretHibet :: Eff HibetEffects a
  -> Logger
  -> IO (Either (CallStack, HibetError) a)
interpretHibet program logger =
  program
    & runConcurrent
    & runResource
    & runConsole
    & runFileSystemIO
    & runPrettyPrint
    & runError @HibetError
    & runLog "hibet" logger defaultLogLevel
    & runEff

hibet :: Eff HibetEffects ()
hibet = do
    mv <- newEmptyMVar
    runReader mv $ withAsync prepareEnv $ \_ -> do
      com <- execParser parser
      runCommand com

prepareEnv ::
  ( FileSystem :> es
  , Error HibetError :> es
  , Reader (MVar Env) :> es
  , Concurrent :> es
  ) => Eff es ()
prepareEnv = do
  !env <- makeEnv
  putEnvMVar env

handleHibetError ::
     Either (CallStack, HibetError) a
  -> IO ()
handleHibetError = \case
  Right _ -> pure ()
  Left (stack, err) -> do
    TIO.putStrLn "Hibet application failed with exception:"
    print err
    TIO.putStrLn "The stack is:"
    System.IO.putStrLn $ prettyCallStack stack

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
-- {-# NOINLINE withLogger #-}
