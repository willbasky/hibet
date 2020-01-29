module Hibet.Interpretator
  ( runHibet
  )
  where

import Hibet.Language
import Pretty

import Control.Monad (when)
import Control.Monad.Free.Church
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Prettyprint.Doc (LayoutOptions (..), PageWidth (..), defaultLayoutOptions,
                                  layoutSmart)
import Data.Text.Prettyprint.Doc.Render.Terminal (renderStrict)
import Paths_hibet (getDataFileName)
import System.Console.Haskeline.IO
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitSuccess)
import System.Pager (printOrPage)
import Path (parseAbsDir)
import Path.IO (listDir)



interpretHibetMethod :: HibetMethod a -> IO a

interpretHibetMethod (PutColorText col ln txt x) = x <$> putColorDoc col ln txt


interpretHibetMethod Exit = exitSuccess


interpretHibetMethod (QueryInput state input x) = x <$> queryInput state input


interpretHibetMethod (PrettyPrint doc x) = fmap x $ do
  -- enable colors in `less`
  lessConf <- lookupEnv "LESS"
  when (isNothing lessConf) $ setEnv "LESS" "-R"
  width' <- maybe 80 Terminal.width <$> Terminal.size
  let layoutOptions =
        defaultLayoutOptions {layoutPageWidth = AvailablePerLine width' 1}
  printOrPage . (`T.snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc


interpretHibetMethod (GetContent path x) = fmap x $ T.decodeUtf8 <$> BS.readFile path


interpretHibetMethod (GetDataFileName path x) = x <$> getDataFileName path


interpretHibetMethod (ListDirectory path x) = fmap x $ do
  dir <-  parseAbsDir path
  listDir dir


runHibet :: Hibet r -> IO r
runHibet = foldF interpretHibetMethod
