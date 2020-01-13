{-# LANGUAGE DeriveAnyClass #-}

module Types
  ( Env(..)
  , Query
  , Dictionary
  , DictionaryMeta (..)
  , Title
  , Source
  , Target
  )
  where


import Parse (WylieTibet, TibetWylie)

import Data.RadixTree (RadixTree)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)


type Title = Text
type Source = Text
type Target = Text

type Dictionary = HashMap Source Target -- | key and value

data DictionaryMeta = DictionaryMeta
  { dmDictionary :: Dictionary
  , dmTitle      :: Text
  , dmNumber     :: Int
  } deriving (Generic, NFData)

-- | Environment fot translator
data Env = Env
  { envDictionaryMeta :: ![DictionaryMeta]
  , envWylieTibet     :: !WylieTibet
  , envTibetWylie     :: !TibetWylie
  , envRadixWylie     :: !RadixTree
  , envRadixTibet     :: !RadixTree
  }

type Query = Text
