{-
Tibetan spelling grammar 4.17 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint17
    ( pConstraint17Ra
    , pConstraint17Ya
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token
  ( Consonant (..)
  , Token
  , TokenCanonical (TcConsonant)
  , tokenCanonical
  )
import qualified Text.Megaparsec as MP

-- (1) root group [ 'ག', 'ད' ] above the subfix ར.
pConstraint17Ra :: Spelling -> Parser TibetanWord
pConstraint17Ra spelling = do
  root <- mark Root (pRootConsonant [Cg, Cd])
  ra <- mark Subfix GP.pSubfixRa
  wa <- mark Subfix GP.pSubfixWa
  vowel <- MP.optional (vowelSlot spelling)
  pure (root <> ra <> wa <> fromMaybe mempty vowel)

-- (2) root ཕ above the subfix ཡ.
pConstraint17Ya :: Spelling -> Parser TibetanWord
pConstraint17Ya spelling = do
  root <- mark Root (pRootConsonant [Cph])
  ya <- mark Subfix GP.pSubfixYa
  wa <- mark Subfix GP.pSubfixWa
  vowel <- MP.optional (vowelSlot spelling)
  pure (root <> ya <> wa <> fromMaybe mempty vowel)

pRootConsonant :: [Consonant] -> Parser Token
pRootConsonant allowed = do
  tok <- GP.pConsonant
  case tokenCanonical tok of
    TcConsonant c | c `elem` allowed -> pure tok
    _ -> MP.empty