module Effects.Console where

import Type (HibetError(..))
import Effects.Common (adapt)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Opt
import qualified System.Console.Haskeline as Haskeline
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Haskeline
import qualified System.Exit as Exit

import Effectful.TH ( makeEffect )
import Effectful ( type (:>), Effect, Eff, IOE )
import Effectful.Error.Static ( Error )
import Effectful.Dispatch.Dynamic ( interpret )

data Console :: Effect where
  GetInput :: InputState -> String -> Console m (Maybe String)
  CloseInput :: InputState -> Console m ()
  GetHistory :: InputState -> Console m History
  ExitSuccess :: Console m ()
  ExecParser :: ParserInfo a -> Console m a

makeEffect ''Console

runConsole ::
  (  IOE :> es
  ,  Error HibetError :> es
  )
  => Eff (Console : es) a
  -> Eff es a
runConsole = interpret $ \_ -> \case
  GetInput state str ->
    adapt $ Haskeline.queryInput state $ Haskeline.getInputLine str
  CloseInput state -> adapt $ Haskeline.closeInput state
  GetHistory state -> adapt $ Haskeline.queryInput state Haskeline.getHistory
  ExitSuccess -> adapt Exit.exitSuccess
  ExecParser info ->  adapt $ Opt.execParser info
