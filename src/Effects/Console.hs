module Effects.Console where

import Polysemy (Embed, Member, Sem)
import qualified Polysemy as P
import qualified System.Console.Haskeline as Console
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Console
import qualified System.Exit as Exit
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Opt


data Console m a where
  InitializeInput :: Console m InputState
  GetInput :: InputState -> String -> Console m (Maybe String)
  CancelInput :: InputState -> Console m ()
  CloseInput :: InputState -> Console m ()
  GetHistory :: InputState -> Console m History
  ExitSuccess :: Console m ()
  ExecParser :: ParserInfo a -> Console m a

P.makeSem ''Console

runConsole :: Member (Embed IO) r
  => Sem (Console : r) a
  -> Sem r a
runConsole = P.interpret $ \case
  InitializeInput -> P.embed $ Console.initializeInput Console.defaultSettings
  GetInput state str ->
    P.embed $ Console.queryInput state $ Console.getInputLine str
  CancelInput state -> P.embed $ Console.cancelInput state
  CloseInput state -> P.embed $ Console.closeInput state
  GetHistory state -> P.embed $ Console.queryInput state Console.getHistory
  ExitSuccess -> P.embed Exit.exitSuccess
  ExecParser info ->  P.embed $ Opt.execParser info





