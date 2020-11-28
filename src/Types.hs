{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where


import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.Reader (ReaderT)
import Data.HashMap.Strict (HashMap)
import Data.RadixTree (RadixTree)
import Data.Text (Text)
import Data.Vector

import qualified Data.Set as Set



type Hibet = ReaderT Env IO
type Title = Text
type Source = Text
type Target = Text
type Answer = ([Target], (Title, Int))

data Line = NewLine | CurrentLine

type Dictionary = HashMap Source Target -- key and value

data DictionaryMeta = DictionaryMeta
  { dictionary :: !Dictionary
  , title      :: !Text
  , number     :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Environment fot translator
data Env = Env
  { dictionaryMeta :: ![DictionaryMeta]
  , wylieTibet     :: !WylieTibet
  , tibetWylie     :: !TibetWylie
  , radixWylie     :: !RadixTree
  , radixTibet     :: !RadixTree
  , labels         :: !Labels
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)


type Query = Text
type QueryWylie = Text

data LabelFull = LabelFull
    { path      :: !Text
    , lfId      :: !Int
    , label     :: !Text
    , about     :: !Text
    , available :: !Bool
    , source    :: !Text
    , target    :: !(Set.Set Text)
    , year      :: !(Maybe Int)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)


newtype Labels = Labels
    { labelTitles :: [LabelFull]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

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

-- | Prepare syllables hashmaps.
type WylieTibet = HashMap Wylie Tibet
type TibetWylie = HashMap Tibet Wylie

type Wylie = Text
type Tibet = Text
