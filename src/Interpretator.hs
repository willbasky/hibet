module Interpretator
  ( runHibet
  )
  where

import Handlers (searchTranslation, separator, sortOutput)
import Language
import Parse
import Pretty
import Types

import Control.Monad.Free.Church
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Console.Haskeline.IO
import System.Exit (exitSuccess)
import qualified Text.Megaparsec.Error as ME


interpretHibetMethod :: HibetMethod a -> IO a

interpretHibetMethod (PutColorText col txt x) = x <$> putColorDoc col txt

interpretHibetMethod Exit = exitSuccess

interpretHibetMethod (QueryInput state input x) = x <$> queryInput state input

interpretHibetMethod (Translate query env x) = fmap x $ do
  let toWylie' = toWylie (envTibetWylie env) . parseTibetanInput (envRadixTibet env)
  let wylieQuery = case toWylie' query  of
          Left _      -> query
          Right wylie -> if T.null wylie then query else wylie
  dscMaybeValues <- traverse (pure . searchTranslation wylieQuery) (envDictionaryMeta env)
  let dscValues = catMaybes dscMaybeValues
  if null dscValues then putColorDoc red "Nothing found."
  else do
      let dictMeta = sortOutput dscValues
      let toTibetan' = toTibetan (envWylieTibet env) . parseWylieInput (envRadixWylie env)
      case traverse (separator [37] toTibetan') dictMeta of
          Left err -> putStrLn $ ME.errorBundlePretty err
          Right list -> do
              let translations = viewTranslations list
              let eitherQuery = if query == wylieQuery
                      then T.concat <$> toTibetan' wylieQuery
                      else Right wylieQuery
              case eitherQuery of
                  Left err     -> putStrLn $ ME.errorBundlePretty err
                  Right query' -> pprint $ withHeaderSpaces yellow query' translations



runHibet :: Hibet r -> IO r
runHibet = foldF interpretHibetMethod
