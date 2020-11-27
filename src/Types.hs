{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Types where


import Parse (TibetWylie, WylieTibet)

import Control.Monad.Reader (ReaderT)
import Data.HashMap.Strict (HashMap)
import Data.RadixTree (RadixTree)
import Data.Text (Text)

import qualified Data.Set as Set



type Hibet = ReaderT Env IO
type Title = Text
type Source = Text
type Target = Text
type Answer = ([Target], (Title, Int))

data Line = NewLine | CurrentLine

type Dictionary = HashMap Source Target -- key and value

data DictionaryMeta = DictionaryMeta
  { dictionary :: Dictionary
  , title      :: Text
  , number     :: Int
  }

-- | Environment fot translator
data Env = Env
  { dictionaryMeta :: ![DictionaryMeta]
  , wylieTibet     :: !WylieTibet
  , tibetWylie     :: !TibetWylie
  , radixWylie     :: !RadixTree
  , radixTibet     :: !RadixTree
  , labels         :: !Labels
  }

type Query = Text
type QueryWylie = Text

data LabelFull = LabelFull
    { path      :: Text
    , lfId      :: Int
    , label     :: Text
    , about     :: Text
    , available :: Bool
    , source    :: Text
    , target    :: Set.Set Text
    , year      :: Maybe Int
    }
    deriving stock (Eq, Show)

newtype Labels = Labels
    { labelTitles :: [LabelFull]
    }
    deriving stock (Eq, Show)

----------------------------------------------------------------------------
-- Command data types
----------------------------------------------------------------------------

-- | Represent all available commands
data Command
    -- | @shell@ command launch translating shell
    = Shell Select
    | Om
    | ShowOption Opt

-- | Commands parsed with @show@ command
data Opt = Names | Meta (Maybe Int)

type Select = [Int]
