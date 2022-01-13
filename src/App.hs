module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (Env, makeEnv)
import Utility (debugEnabledEnvVar)

import Data.Function ((&))
import Polysemy (Embed, Members, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputSem)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, traceToStdout, ignoreTrace)



app :: IO ()
app = do
  isDebug <- debugEnabledEnvVar
  res <- interpretHibet hibet isDebug
  case res of
    Right _ -> pure ()
    Left err -> do
      putStrLn "Hibet application failed with exception:"
      print err

interpretHibet :: Sem
  '[  Input Env
    , FileIO
    , Error HibetError
    , Resource
    , Console
    , PrettyPrint
    , Trace
    , Embed IO
    ] ()
  -> Bool -- isDebug
  -> IO (Either HibetError ())
interpretHibet program isDebug = program
  & runInputSem makeEnv
  & runFile
  & runError @HibetError
  & runResource
  & runConsole
  & runPrettyPrint
  & (if isDebug then traceToStdout else ignoreTrace)
  & runM

hibet :: Members
  [ FileIO
  , Error HibetError
  , Input Env
  , Resource
  , Console
  , PrettyPrint
  , Trace
  , Embed IO
  ] r
  => Sem r ()
hibet = do
  -- env <- makeEnv
  com <- execParser parser
  runCommand com
