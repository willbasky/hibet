{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parse.Type where

import Type (HibetError (..))

import Control.Monad.Except (Except, liftEither)
import Control.Parallel.Strategies (NFData)
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M

dot :: Text
dot = "་"

end :: Text
end = "།"

space :: Text
space = " "

newtype Script scripttype = Script Text
  deriving newtype (Eq, Show)
  deriving (NFData, Hashable, Ord) via Text

data ScriptType = Tibet | Wylie

class ScriptSeparator (s :: ScriptType) where
  separator :: Text

instance ScriptSeparator 'Tibet where
  separator = dot

instance ScriptSeparator 'Wylie where
  separator = space

fromScripts :: forall s. ScriptSeparator s
  => [Script s]
  -> Text
fromScripts = T.intercalate (separator @s) . coerce

-- | I refused a Bimap.
-- The reason is
-- W -> T not isomorphic to T -> W therefore
-- Bimap lost some pairs, hence it is better to use wylie within input.
type WylieTibetMap = HashMap (Script 'Wylie) (Script 'Tibet)
type TibetWylieMap = HashMap (Script 'Tibet) (Script 'Wylie)


type Parser a = M.Parsec Void Text a

parseExcept :: Parser a -> Text -> Except HibetError a
parseExcept p t = liftEither $ mapLeft MegaError $ M.runParser p "" t

parseEither :: Parser a -> Text -> Either HibetError a
parseEither p t = mapLeft MegaError $ M.runParser p "" t
