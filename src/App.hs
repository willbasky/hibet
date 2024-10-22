{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

module App
       ( app
       ) where

import Cli (parser, runCommand)
import Env (Env, makeEnv)
import Type (HibetError (..))
import qualified Data.Text.IO as TIO
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
import Effectful.Concurrent.MVar ( MVar, newEmptyMVar, putMVar )
import Effectful.Reader.Dynamic ( runReader )
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
  errs <- withStdOutLogger $ \logger -> do
    interpretHibet hibet logger
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
  withAsync (prepareEnv mv) $ \_ ->
    runReader mv $ do
      com <- execParser parser
      runCommand com

prepareEnv ::
  ( FileSystem :> es
  , Error HibetError :> es
  , Concurrent :> es
  ) => MVar Env -> Eff es ()
prepareEnv mv = do
  !env <- makeEnv
  putMVar mv env

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
