module Convert.Tokenizer.Unicode where

import Convert.Token
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Control.Applicative (asum)

-- | Tokenize Unicode Tibetan input to typed IR tokens.
tokenizeUnicode :: Text -> [Token]
tokenizeUnicode input = go 0 input
  where
    go _ rest | T.null rest = []
    go offset rest =
        case T.uncons rest of
            Nothing -> []
            Just (c, next) ->
                let raw = T.singleton c
                    end = offset + 1
                    span = mkSpan (fromIntegral offset) (fromIntegral end)
                 in classifyChar span c raw : go end next

classifyChar :: Span -> Char -> Text -> Token
classifyChar span ch raw =
    fromMaybe (mkUnknown TsUnicode span raw) $ asum
        [ fromMap mkConsonant consonantTokenMap
        , fromMap mkSubConsonant subConsonantTokenMap
        , fromMap mkVowel vowelTokenMap
        , fromMap mkFinal finalTokenMap
        , fromMap mkNumber numberTokenMap
        , fromMap mkPunctuation punctuationTokenMap
        , fromMap mkSymbol symbolTokenMap
        , if ch == ' '
            then Just (mkSpace TsUnicode span raw SMSpace)
            else Nothing
        ]
  where
    fromMap constructor tokenMap =
        constructor TsUnicode span raw <$> HM.lookup ch tokenMap


consonantTokenMap :: HashMap Char Consonant
consonantTokenMap =
    HM.fromList
        [ ('\x0f40', Ck)
        , ('\x0f41', Ckh)
        , ('\x0f42', Cg)
        , ('\x0f43', CgPLUSh)
        , ('\x0f44', Cng)
        , ('\x0f45', Cc)
        , ('\x0f46', Cch)
        , ('\x0f47', Cj)
        , ('\x0f49', Cny)
        , ('\x0f4a', CT)
        , ('\x0f4b', CTh)
        , ('\x0f4c', CD)
        , ('\x0f4d', CDPLUSh)
        , ('\x0f4e', CN)
        , ('\x0f4f', Ct)
        , ('\x0f50', Cth)
        , ('\x0f51', Cd)
        , ('\x0f52', CdPLUSh)
        , ('\x0f53', Cn)
        , ('\x0f54', Cp)
        , ('\x0f55', Cph)
        , ('\x0f56', Cb)
        , ('\x0f57', CbPLUSh)
        , ('\x0f58', Cm)
        , ('\x0f59', Cts)
        , ('\x0f5a', Ctsh)
        , ('\x0f5b', Cdz)
        , ('\x0f5c', CdzPLUSh)
        , ('\x0f5d', Cw)
        , ('\x0f5e', Czh)
        , ('\x0f5f', Cz)
        , ('\x0f60', C')
        , ('\x0f61', Cy)
        , ('\x0f62', Cr)
        , ('\x0f63', Cl)
        , ('\x0f64', Csh)
        , ('\x0f65', CSh)
        , ('\x0f66', Cs)
        , ('\x0f67', Ch)
        , ('\x0f68', Ca)
        , ('\x0f69', CkPLUSSh)
        , ('\x0f6a', CR)
        ]

subConsonantTokenMap :: HashMap Char SubConsonant
subConsonantTokenMap =
    HM.fromList
        [ ('\x0f90', SCk)
        , ('\x0f91', SCkh)
        , ('\x0f92', SCg)
        , ('\x0f93', SCgPLUSh)
        , ('\x0f94', SCng)
        , ('\x0f95', SCc)
        , ('\x0f96', SCch)
        , ('\x0f97', SCj)
        , ('\x0f99', SCny)
        , ('\x0f9a', SCT)
        , ('\x0f9b', SCTh)
        , ('\x0f9c', SCD)
        , ('\x0f9d', SCDPLUSh)
        , ('\x0f9e', SCN)
        , ('\x0f9f', SCt)
        , ('\x0fa0', SCth)
        , ('\x0fa1', SCd)
        , ('\x0fa2', SCdPLUSh)
        , ('\x0fa3', SCn)
        , ('\x0fa4', SCp)
        , ('\x0fa5', SCph)
        , ('\x0fa6', SCb)
        , ('\x0fa7', SCbPLUSh)
        , ('\x0fa8', SCm)
        , ('\x0fa9', SCts)
        , ('\x0faa', SCtsh)
        , ('\x0fab', SCdz)
        , ('\x0fac', SCdzPLUSh)
        , ('\x0fad', SCw)
        , ('\x0fae', SCzh)
        , ('\x0faf', SCz)
        , ('\x0fb0', SC')
        , ('\x0fb1', SCy)
        , ('\x0fb2', SCr)
        , ('\x0fb3', SCl)
        , ('\x0fb4', SCsh)
        , ('\x0fb5', SCSh)
        , ('\x0fb6', SCs)
        , ('\x0fb7', SCh)
        , ('\x0fb8', SCa)
        , ('\x0fb9', SCkPLUSSh)
        , ('\x0fba', SCW)
        , ('\x0fbb', SCY)
        , ('\x0fbc', SCR)
        ]

vowelTokenMap :: HashMap Char Vowel
vowelTokenMap =
    HM.fromList
        [ ('\x0f71', VA)
        , ('\x0f72', Vi)
        , ('\x0f73', VI)
        , ('\x0f74', Vu)
        , ('\x0f75', VU)
        , ('\x0f7a', Ve)
        , ('\x0f7b', Vai)
        , ('\x0f7c', Vo)
        , ('\x0f7d', Vau)
        , ('\x0f80', V_i)
        ]

finalTokenMap :: HashMap Char FinalMark
finalTokenMap =
    HM.fromList
        [ ('\x0f7e', FMAnusvara)
        , ('\x0f82', FMAnusvara)
        , ('\x0f83', FMAnusvara)
        , ('\x0f37', FMCandrabinduOrNasal)
        , ('\x0f35', FMCandrabinduOrNasal)
        , ('\x0f39', FMCaret)
        , ('\x0f7f', FMVisarga)
        , ('\x0f84', FMHalanta)
        , ('\x0f85', FMYigMgo)
        ]

numberTokenMap :: HashMap Char Number
numberTokenMap =
    HM.fromList
        [ ('\x0f20', N0)
        , ('\x0f21', N1)
        , ('\x0f22', N2)
        , ('\x0f23', N3)
        , ('\x0f24', N4)
        , ('\x0f25', N5)
        , ('\x0f26', N6)
        , ('\x0f27', N7)
        , ('\x0f28', N8)
        , ('\x0f29', N9)
        ]

punctuationTokenMap :: HashMap Char PunctuationMark
punctuationTokenMap =
    HM.fromList
        [ ('\x0f0b', PMTsheg)
        , ('\x0f0c', PMNonBreakingTsheg)
        , ('\x0f0d', PMShad)
        , ('\x0f0e', PMNyisShad)
        , ('\x0f0f', PMTshegShad)
        , ('\x0f11', PMRinChenSpungsShad)
        , ('\x0f14', PMGterTshigMgo)
        ]

symbolTokenMap :: HashMap Char SymbolMark
symbolTokenMap =
    HM.fromList
        [ ('\x0f08', SMExclamation)
        , ('\x0f04', SMAt)
        , ('\x0f05', SMHash)
        , ('\x0f06', SMDollar)
        , ('\x0f07', SMPercent)
        , ('\x0f34', SMEqual)
        , ('\x0f3a', SMLt)
        , ('\x0f3b', SMGt)
        , ('\x0f3c', SMLParen)
        , ('\x0f3d', SMRParen)
        ]



