{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint08 (pConstraint08) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token (SubConsonant (..), Token, TokenCanonical (TcSubConsonant), tokenCanonical)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

pConstraint08 :: Spelling -> Parser TibetanWord
pConstraint08 spelling =
    MP.choice
        [ MP.try $ parseConstraint08 spelling GP.pSuperfixRa pRaSuperfixRoot
        , MP.try $ parseConstraint08 spelling GP.pSuperfixLa pLaSuperfixRoot
        , MP.try $ parseConstraint08 spelling GP.pSuperfixSa pSaSuperfixRoot
        ]

parseConstraint08 :: Spelling -> Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint08 spelling parseSuperfix parseRoot = do
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.optional (vowelSlot spelling)
    pure (superfix <> root <> fromMaybe mempty vowel)

pRaSuperfixRoot :: Parser Token
pRaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCj, SCny, SCt, SCd, SCn, SCb, SCm, SCts, SCdz ]

pLaSuperfixRoot :: Parser Token
pLaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCc, SCj, SCt, SCd, SCp, SCb, SCh ]

pSaSuperfixRoot :: Parser Token
pSaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCny, SCt, SCd, SCn, SCp, SCb, SCm, SCts ]

pAllowedSubConsonant :: [SubConsonant] -> Parser Token
pAllowedSubConsonant allowed = do
    tok <- GP.pSubConsonant
    case tokenCanonical tok of
        TcSubConsonant sc
            | sc `elem` allowed -> pure tok
        _ -> MP.empty
