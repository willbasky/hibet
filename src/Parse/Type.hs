{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Parse.Type where

import Type (HibetError (..))

import Control.Monad.Except (Except, liftEither)
import Control.Parallel.Strategies (NFData)
import Data.Either.Extra (mapLeft)
import Data.Hashable ( Hashable )
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.List (foldl')
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M


-- | For transcripton
data WylieScript
  = ScriptWylie WylieSyllable
  | NonScriptWylie Text
  deriving stock (Show, Eq)

newtype WylieSyllable = WylieSyllable {unWylie :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (Ord) via Text
  deriving anyclass (NFData, Hashable)

fromWylieScript :: [WylieScript] -> Either HibetError Text
fromWylieScript wss = if null nsw
  then Right unSW
  else Left $ NotSyllable unNSW
  where
    (sw, nsw) = foldl' foldHelp ([],[]) wss
    foldHelp (s,n) (ScriptWylie ws) = (unWylie ws : s, n)
    foldHelp (s,n) (NonScriptWylie t) = (s, t:n)
    unNSW = T.intercalate " " $ reverse nsw
    unSW = T.intercalate " " $ reverse sw

data TibetScript
  = ScriptTibet TibetSyllable
  | NonScriptTibet Text
  deriving stock (Show, Eq)

newtype TibetSyllable = TibetSyllable {unTibet :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (Ord) via Text
  deriving anyclass (NFData, Hashable)

fromTibetScript :: [TibetScript] -> Either HibetError Text
fromTibetScript tss = if null nst
  then Right unST
  else Left $ NotSyllable unNST
  where
    (st, nst) = foldl' foldHelp ([],[]) tss
    foldHelp (t,n) (ScriptTibet ts) = (unTibet ts : t, n)
    foldHelp (s,n) (NonScriptTibet txt) = (s, txt:n)
    unNST = T.intercalate " " $ reverse nst
    unST = T.intercalate " " $ reverse st

-- | I refused a Bimap.
-- The reason is
-- W -> T not isomorphic to T -> W therefore
-- Bimap lost some pairs, hence it is better to use wylie within input.
type WylieTibetMap = HashMap WylieSyllable TibetSyllable
type TibetWylieMap = HashMap TibetSyllable WylieSyllable


type Parser a = M.Parsec Void Text a

parseExcept :: Parser a -> Text -> Except HibetError a
parseExcept p t = liftEither $ mapLeft MegaError $ M.runParser p "" t

parseEither :: Parser a -> Text -> Either HibetError a
parseEither p t = mapLeft MegaError $ M.runParser p "" t
