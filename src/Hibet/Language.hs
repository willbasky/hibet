{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hibet.Language
  ( HibetMethod(..)
  , Hibet

  , exitH
  , putColorTextH
  , queryInputH
  , pprintH
  )
  where


import Control.Monad.Free.Church
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline.IO (InputState)




data HibetMethod next where

  Exit :: HibetMethod next

  PutColorText :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> (() -> next) -> HibetMethod next

  PrettyPrint :: Doc AnsiStyle -> (() -> next) -> HibetMethod next

  QueryInput :: forall a next . InputState -> InputT IO a -> (a -> next) -> HibetMethod next


instance Functor HibetMethod where
  fmap f (PutColorText col txt next)   = PutColorText col txt (f . next)

  fmap _ Exit                          = Exit

  fmap f (PrettyPrint doc next)        = PrettyPrint doc (f . next)

  fmap f (QueryInput state input next) = QueryInput state input (f . next)




type Hibet = F HibetMethod

exitH :: Hibet ()
exitH = liftF Exit

putColorTextH :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> Hibet ()
putColorTextH col str = liftF $ PutColorText col str id

pprintH :: Doc AnsiStyle -> Hibet ()
pprintH doc = liftF $ PrettyPrint doc id

queryInputH :: forall a . InputState -> InputT IO a -> Hibet a
queryInputH state input = liftF $ QueryInput state input id
