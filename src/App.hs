module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Compact
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (EnvC, makeEnv)
import Type (HibetError (..))
import Utility (debugEnabledEnvVar)

import Data.Function ((&))
import Polysemy (Embed, Members, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Resource (Resource, runResource)
import Polysemy.Trace (Trace, ignoreTrace, traceToStdout)


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
      Reader EnvC
    , FileIO
    , Error HibetError
    , Resource
    , CompactData
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
  & runCompactData
  & runConsole
  & runPrettyPrint
  & (if isDebug then traceToStdout else ignoreTrace)
  & runM

hibet :: Members
  [
    Reader EnvC
  , FileIO
  , Error HibetError
  , Resource
  , CompactData
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
