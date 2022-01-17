module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (Env, makeEnv)
import Utility (debugEnabledEnvVar)
import Type (HibetError (..))

import Data.Function ((&))
import Polysemy (Embed, Members, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
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
  '[
      Reader Env
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
  & runReaderSem makeEnv
  & runFile
  & runError @HibetError
  & runResource
  & runConsole
  & runPrettyPrint
  & (if isDebug then traceToStdout else ignoreTrace)
  & runM

hibet :: Members
  [
    Reader Env
  , FileIO
  , Error HibetError
  , Resource
  , Console
  , PrettyPrint
  , Trace
  , Embed IO
  ] r
  => Sem r ()
hibet = do
  com <- execParser parser
  runCommand com

runReaderSem :: forall i r a. Sem r i -> Sem (Reader i ': r) a -> Sem r a
runReaderSem env sem = env >>= flip runReader sem
