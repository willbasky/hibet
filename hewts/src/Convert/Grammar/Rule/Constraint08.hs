{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint08 (pConstraint08) where

import qualified Convert.Grammar.Parser as GP
import Convert.Token (SubConsonant (..), Token, TokenCanonical (TcSubConsonant), tokenCanonical)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Data.Maybe (maybeToList)

pConstraint08 :: GP.Parser [Token]
pConstraint08 =
    MP.choice
        [ MP.try $ parseConstraint08 GP.pSuperfixRa pRaSuperfixRoot
        , MP.try $ parseConstraint08 GP.pSuperfixLa pLaSuperfixRoot
        , MP.try $ parseConstraint08 GP.pSuperfixSa pSaSuperfixRoot
        ]

parseConstraint08 :: GP.Parser Token -> GP.Parser Token -> GP.Parser [Token]
parseConstraint08 parseSuperfix parseRoot = do
    superfix <- parseSuperfix
    root <- parseRoot
    vowel <- MP.optional GP.pVowel
    pure $ [superfix, root] <> maybeToList vowel

pRaSuperfixRoot :: GP.Parser Token
pRaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCj, SCny, SCt, SCd, SCn, SCb, SCm, SCts, SCdz ]

pLaSuperfixRoot :: GP.Parser Token
pLaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCc, SCj, SCt, SCd, SCp, SCb, SCh ]

pSaSuperfixRoot :: GP.Parser Token
pSaSuperfixRoot =
    pAllowedSubConsonant
        [ SCk, SCg, SCng, SCny, SCt, SCd, SCn, SCp, SCb, SCm, SCts ]

pAllowedSubConsonant :: [SubConsonant] -> GP.Parser Token
pAllowedSubConsonant allowed = do
    tok <- GP.pSubConsonant
    case tokenCanonical tok of
        TcSubConsonant sc
            | sc `elem` allowed -> pure tok
        _ -> MP.empty
