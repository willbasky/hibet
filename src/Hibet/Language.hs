{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hibet.Language
  ( HibetMethod(..)
  , Hibet

  , exitH
  , getContentH
  , getDataFileNameH
  , listDirectoryH
  , pprintH
  , putColorTextH
  , queryInputH
  )
  where


import Control.Monad.Free.Church
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Path (Abs, Dir, File, Path)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline.IO (InputState)

import Types


data HibetMethod next where

  Exit :: HibetMethod next

  PutColorText :: (Doc AnsiStyle -> Doc AnsiStyle) -> Line -> Text -> (() -> next) -> HibetMethod next

  PrettyPrint :: Doc AnsiStyle -> (() -> next) -> HibetMethod next

  QueryInput :: forall a next . InputState -> InputT IO a -> (a -> next) -> HibetMethod next

  GetContent :: FilePath -> (Text -> next) -> HibetMethod next

  GetDataFileName :: FilePath -> (FilePath -> next) -> HibetMethod next

  ListDirectory :: FilePath -> (([Path Abs Dir], [Path Abs File]) -> next) -> HibetMethod next


instance Functor HibetMethod where
  fmap f (PutColorText col ln txt next) = PutColorText col ln txt (f . next)

  fmap _ Exit                           = Exit

  fmap f (PrettyPrint doc next)         = PrettyPrint doc (f . next)

  fmap f (QueryInput state input next)  = QueryInput state input (f . next)

  fmap f (GetContent path next)         = GetContent path (f . next)

  fmap f (GetDataFileName path next)    = GetDataFileName path (f . next)

  fmap f (ListDirectory path next)      = ListDirectory path (f . next)




type Hibet = F HibetMethod

exitH :: Hibet ()
exitH = liftF Exit

putColorTextH :: (Doc AnsiStyle -> Doc AnsiStyle) -> Line -> Text -> Hibet ()
putColorTextH col ln txt = liftF $ PutColorText col ln txt id

pprintH :: Doc AnsiStyle -> Hibet ()
pprintH doc = liftF $ PrettyPrint doc id

queryInputH :: forall a . InputState -> InputT IO a -> Hibet a
queryInputH state input = liftF $ QueryInput state input id

getContentH :: FilePath -> Hibet Text
getContentH path = liftF $ GetContent path id

getDataFileNameH :: FilePath -> Hibet FilePath
getDataFileNameH path = liftF $ GetDataFileName path id

listDirectoryH :: FilePath -> Hibet ([Path Abs Dir], [Path Abs File])
listDirectoryH path = liftF $ ListDirectory path id
