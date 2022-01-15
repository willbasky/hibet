{-# LANGUAGE DeriveAnyClass #-}

module Type
  (
    -- * Error
    HibetError (..)
  ) where

import Polysemy.Path (PathException)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Text (Text)
import Data.Void (Void)
import Control.Exception (SomeException)


data HibetError
  = PathError PathException
  | MegaError (ParseErrorBundle Text Void)
  | BimapError SomeException
  | UnknownError Text
  deriving stock (Show)
  deriving anyclass (Eq)

-- deriving instance Eq SomeException
