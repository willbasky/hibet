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
import Data.Void (Void)
import GHC.Generics (Generic)
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M


-- | For transcripton
data WylieScript
  = ScriptWylie WylieSyllable
  | NonScriptWylie Text
  deriving stock (Show, Eq)

fromWylieScript :: WylieScript -> Text
fromWylieScript (ScriptWylie s)    = unWylie s
fromWylieScript (NonScriptWylie t) = t

data TibetScript
  = ScriptTibet TibetSyllable
  | NonScriptTibet Text
  deriving stock (Show, Eq)

fromTibetScript :: TibetScript -> Text
fromTibetScript (ScriptTibet s)    = unTibet s
fromTibetScript (NonScriptTibet t) = t

newtype WylieSyllable = WylieSyllable {unWylie :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (Ord) via Text
  deriving anyclass (NFData, Hashable)

newtype TibetSyllable = TibetSyllable {unTibet :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (Ord) via Text
  deriving anyclass (NFData, Hashable)


-- | I refused a Bimap The reason is
-- W -> T not isomorphic to T -> W therefore
-- Bimap lost some pairs.
type WylieTibetMap = HashMap WylieSyllable TibetSyllable
type TibetWylieMap = HashMap TibetSyllable WylieSyllable


type Parser a = M.Parsec Void Text a

parseExcept :: Parser a -> Text -> Except HibetError a
parseExcept p t = liftEither $ mapLeft MegaError $ M.runParser p "" t

parseEither :: Parser a -> Text -> Either HibetError a
parseEither p t = mapLeft MegaError $ M.runParser p "" t
