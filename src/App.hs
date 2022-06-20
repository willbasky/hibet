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

import Data.Function ((&))
import IncipitCore (Async, asyncToIOFinal)
import Polysemy (Embed, Final, Members, Sem, embedToFinal, runFinal)
import Polysemy.Conc (Race, Sync, interpretRace, interpretSync, withAsync_)
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, ignoreTrace, traceToStdout)


app :: IO ()
app = do
  isDebug <- debugEnabledEnvVar
  handleHibetError =<< interpretHibet hibet isDebug

type HibetEffects =
  [
    FileIO
  , Error HibetError
  , Resource
  , Console
  , PrettyPrint
  , Trace
  , Sync Env
  , Race
  , Async
  , Embed IO
  , Final IO
  ]

interpretHibet :: Sem HibetEffects ()
  -> Bool -- isDebug
  -> IO (Either HibetError ())
interpretHibet program isDebug = program
  & runFile
  & runError @HibetError
  & runResource
  & runConsole
  & runPrettyPrint
  & (if isDebug then traceToStdout else ignoreTrace)
  & interpretSync @Env
  & interpretRace
  & asyncToIOFinal
  & embedToFinal
  & runFinal

hibet :: Members HibetEffects r => Sem r ()
hibet = do
  withAsync_ prepareEnv $ do
    com <- execParser parser
    runCommand com

prepareEnv :: Members
  [ FileIO
  , Error HibetError
  , Trace
  , Sync Env
  , Embed IO
  ] r => Sem r ()
prepareEnv = do
  !env <- makeEnv
  putEnvMVar env

handleHibetError :: Either HibetError a -> IO ()
handleHibetError = \case
  Right _ -> pure ()
  Left err -> do
    putStrLn "Hibet application failed with exception:"
    print err
