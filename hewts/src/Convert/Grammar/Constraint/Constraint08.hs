{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint08 (pConstraint08) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token (SubConsonant (..), Token, TokenCanonical (TcSubConsonant), tokenCanonical)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Data.Maybe (fromMaybe)

pConstraint08 :: Parser TibetanWord
pConstraint08 =
    MP.choice
        [ MP.try $ parseConstraint08 GP.pSuperfixRa pRaSuperfixRoot
        , MP.try $ parseConstraint08 GP.pSuperfixLa pLaSuperfixRoot
        , MP.try $ parseConstraint08 GP.pSuperfixSa pSaSuperfixRoot
        ]

parseConstraint08 :: Parser Token -> Parser Token -> Parser TibetanWord
parseConstraint08 parseSuperfix parseRoot = do
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.optional (mark Vowel GP.pVowel)
    pure (superfix <> root <> fromMaybe [] vowel)

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
