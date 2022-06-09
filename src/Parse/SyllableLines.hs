module Parse.SyllableLines where

import Parse.Type
    ( parseExcept,
      TibetSyllable(TibetSyllable),
      WylieSyllable(WylieSyllable),
      Parser )
import Type ( HibetError(..) )

import Control.Applicative (Alternative (some))
import Control.Monad.Except (Except)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lines as Line
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC


---------------------------------------------------------------------
-- Hashmaps from syllables
---------------------------------------------------------------------

splitSyllables :: Text -> Except HibetError [(WylieSyllable,TibetSyllable)]
splitSyllables
    = traverse (parseExcept parseSyllables)
    . Line.lines
    . Line.fromText

parseSyllables :: Parser (WylieSyllable,TibetSyllable)
parseSyllables = do
    w <- some $ M.anySingleBut '|'
    _ <- MC.char '|'
    t <- some M.anySingle
    pure (WylieSyllable $ T.pack w, TibetSyllable $ T.pack t)
