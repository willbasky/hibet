module Effects.PrettyPrint where

import Pretty

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Embed, Member, Sem)
import qualified Polysemy as P
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), defaultLayoutOptions, layoutSmart,
                      pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc, renderStrict)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv, setEnv)
import System.Pager (printOrPage)
import Data.Foldable (traverse_)

data Line = NewLine | CurrentLine

data PrettyPrint m a where
  PutColorDoc :: Colorize -> Line -> Text -> PrettyPrint m ()
  Pprint :: Doc AnsiStyle -> PrettyPrint m ()

P.makeSem ''PrettyPrint

runPrettyPrint :: Member (Embed IO) r => Sem (PrettyPrint : r) a -> Sem r a
runPrettyPrint = P.interpret $ \case
  PutColorDoc col isNewLine txt -> P.embed $ do
    let txtLn = case isNewLine of
          NewLine     -> txt `T.snoc` '\n'
          CurrentLine -> txt
    putDoc $ col $ pretty txtLn

  Pprint doc -> P.embed $ do
    -- enable colors in `less`
    lessConf <- lookupEnv "LESS"
    when (isNothing lessConf) $ setEnv "LESS" "-R"
    width' <- maybe 80 Terminal.width <$> Terminal.size
    let layoutOptions =
          defaultLayoutOptions {layoutPageWidth = AvailablePerLine width' 1}
    printOrPage . (`T.snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc


putColorList :: Member PrettyPrint r
  => [(Colorize, Text)]
  -> Sem r ()
putColorList = traverse_ (\(c,d) -> putColorDoc c CurrentLine d)
