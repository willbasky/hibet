module Parse.SyllableLines where

import Parse.Type
    ( parseEither,
      Script(..),
      ScriptType(..),
      Parser )
import Type ( HibetError(..) )

import Control.Applicative (Alternative (some))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lines as Line
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC


---------------------------------------------------------------------
-- Hashmaps from syllables
---------------------------------------------------------------------

splitSyllables :: Text -> Either HibetError [(Script Wylie,Script 'Tibet)]
splitSyllables
    = traverse (parseEither parseSyllables)
    . Line.lines
    . Line.fromText

parseSyllables :: Parser (Script 'Wylie, Script 'Tibet)
parseSyllables = do
    w <- some $ M.anySingleBut '|'
    _ <- MC.char '|'
    t <- some M.anySingle
    pure (Script $ T.pack w, Script $ T.pack t)
