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
    , Error HibetErrors
    , Resource
    , Console
    , PrettyPrint
    , Embed IO
    ] ()
  -> IO (Either HibetErrors ())
interpretHibet program = program
  & runFile
  & runError @HibetErrors
  & runResource
  & runConsole
  & runPrettyPrint
  & runM

hibet :: Members
  [ FileIO
  , Error HibetErrors
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
