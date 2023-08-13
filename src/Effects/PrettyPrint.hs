module Effects.PrettyPrint where

import Pretty
import Type (HibetError(..))
import Utility ( showT )

import Control.Monad (when)
import Control.Exception (SomeException)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), defaultLayoutOptions, layoutSmart,
                      pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc, renderStrict)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv, setEnv)
import System.Pager (printOrPage)
import Data.Foldable (traverse_)

import Effectful.TH ( makeEffect )
import Effectful ( MonadIO(liftIO), type (:>), Effect, Eff, IOE )
import Effectful.Error.Static
    ( CallStack, prettyCallStack, Error, catchError, throwError )
import Effectful.Dispatch.Dynamic ( interpret, localSeqUnliftIO )

data Line = NewLine | CurrentLine

data PrettyPrint :: Effect where
  PutColorDoc :: Colorize -> Line -> Text -> PrettyPrint m ()
  Pprint :: Doc AnsiStyle -> PrettyPrint m ()
  PrintDebug :: Show a => a -> PrettyPrint m ()

makeEffect ''PrettyPrint

runPrettyPrint ::
  (  IOE :> es
  ,  Error HibetError :> es
  ,  Error SomeException :> es
  )
  => Eff (PrettyPrint : es) a
  -> Eff es a
runPrettyPrint = interpret $ \env -> \case
  PutColorDoc col isNewLine txt -> adapt $ do
      let txtLn = case isNewLine of
            NewLine     -> txt `T.snoc` '\n'
            CurrentLine -> txt
      putDoc $ col $ pretty txtLn

  Pprint doc -> localSeqUnliftIO env $ \_ ->  do
    -- enable colors in `less`
    lessConf <- lookupEnv "LESS"
    when (isNothing lessConf) $ setEnv "LESS" "-R"
    width' <- maybe 80 Terminal.width <$> Terminal.size
    let layoutOptions =
          defaultLayoutOptions {layoutPageWidth = AvailablePerLine width' 1}
    printOrPage . (`T.snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc
  PrintDebug str -> adapt $ print str

putColorList :: (PrettyPrint:> es)
  => [(Colorize, Text)]
  -> Eff es ()
putColorList = traverse_ (\(c,d) -> putColorDoc c CurrentLine d)

adapt ::
  ( IOE :> es
  , Error HibetError :> es
  , Error SomeException :> es
  )
  => IO a -> Eff es a
adapt m = catchError (liftIO m) $
  \(stack :: CallStack) (e :: SomeException) -> throwError
      $ UnknownError
      $ showT e
      <> " \nwith stack:\n"
      <> showT (prettyCallStack stack)
