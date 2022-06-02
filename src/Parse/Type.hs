{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Parse.Type where
import Type (HibetError (..))

import Control.Monad.Except (Except, liftEither)
import Control.Parallel.Strategies (NFData)
import Data.Bimap (Bimap)
import Data.Either.Extra (mapLeft)
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
  deriving anyclass (NFData)

newtype TibetSyllable = TibetSyllable {unTibet :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (Ord) via Text
  deriving anyclass (NFData)


-- | Syllable bimap.
type BimapWylieTibet = Bimap WylieSyllable TibetSyllable


type Parser a = M.Parsec Void Text a

parseT :: Parser a -> String -> Text -> Except HibetError a
parseT p s t = liftEither $ mapLeft MegaError $ M.runParser p s t
