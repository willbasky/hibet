{-# LANGUAGE BangPatterns #-}

module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (Env, makeEnv)
import Type (HibetError (..))
import Utility (debugEnabledEnvVar)

import Control.Concurrent
import Control.Exception (SomeException)
import Data.Function ((&))
import Polysemy (Embed, Members, Member, Sem, embed, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, ignoreTrace, traceToStdout)


app :: IO ()
app = do
  isDebug <- debugEnabledEnvVar
  envVar <- newEmptyMVar
  thread <- flip forkFinally handleForkError $
    handleHibetError =<< interpretEnv (putEnv envVar) isDebug
  handleHibetError =<< interpretHibet hibet isDebug envVar
  killThread thread

interpretHibet :: Sem HibetEffects ()
  -> Bool -- isDebug
  -> MVar Env
  -> IO (Either HibetError ())
interpretHibet program isDebug envVar = program
  & runReaderSem envVar
  & runFile
  & runError @HibetError
  & runResource
  & runConsole
  & runPrettyPrint
  & (if isDebug then traceToStdout else ignoreTrace)
  & runM

hibet :: Members HibetEffects r => Sem r ()
hibet = do
  com <- execParser parser
  runCommand com

type HibetEffects =
  [
    Reader Env
  , FileIO
  , Error HibetError
  , Resource
  , Console
  , PrettyPrint
  , Trace
  , Embed IO
  ]

interpretEnv :: Sem EnvEffects ()
  -> Bool -- isDebug
  -> IO (Either HibetError ())
interpretEnv program isDebug = program
  & runFile
  & runError @HibetError
  & (if isDebug then traceToStdout else ignoreTrace)
  & runM

putEnv :: Members EnvEffects r => MVar Env -> Sem r ()
putEnv envVar = do
  !env <- makeEnv
  embed $ putMVar envVar env

type EnvEffects =
  [
    FileIO
  , Error HibetError
  , Trace
  , Embed IO
  ]

runReaderSem :: forall r a. Member (Embed IO) r
  => MVar Env
  -> Sem (Reader Env ': r) a
  -> Sem r a
runReaderSem envVar sem = do
  env <- embed $ takeMVar envVar
  runReader env sem

handleHibetError :: Either HibetError a -> IO ()
handleHibetError = \case
  Right _ -> pure ()
  Left err -> do
    putStrLn "Hibet application failed with exception:"
    print err

handleForkError :: Either SomeException a -> IO ()
handleForkError = \case
  Left err -> putStrLn $ "Error in thread: " <> show err
  Right _ -> do
    pure ()
    -- putStrLn "Env made"
