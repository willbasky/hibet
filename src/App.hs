module App
       ( app
       ) where

import Cli (parser, runCommand)
import Effects.Console
import Effects.File
import Effects.PrettyPrint
import Env (makeEnv)

import Data.Function ((&))
import Polysemy (Embed, Members, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, runResource)
-- import Polysemy.Trace (Trace, traceToStdout)



app :: IO ()
app = do
  res <- interpretHibet hibet
  case res of
    Right _ -> pure ()
    Left err -> do
      putStrLn "Hibet application failed with exception:"
      print err

interpretHibet :: Sem
  '[  FileIO
    , Error HibetError
    , Resource
    , Console
    , PrettyPrint
    -- , Trace
    , Embed IO
    ] ()
  -> IO (Either HibetError ())
interpretHibet program = program
  & runFile
  & runError @HibetError
  & runResource
  & runConsole
  & runPrettyPrint
  -- & traceToStdout
  & runM

hibet :: Members
  [ FileIO
  , Error HibetError
  , Resource
  , Console
  , PrettyPrint
  , Embed IO
  ] r
  => Sem r ()
hibet = do
  env <- makeEnv
  com <- execParser parser
  runCommand env com
