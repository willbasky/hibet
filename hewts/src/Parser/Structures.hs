module Parser.Structures
    (
    ) where

import Parser.Common
import Parser.Rules.Grammar01 (pGrammar1, pGrammar1WithLong, pGrammar1Sanskrit)
import Parser.Rules.Grammar08 (pGrammar8)
import Parser.Rules.Grammar09 (pGrammar9)
import Parser.Rules.Grammar10 (pGrammar10)
import Parser.Rules.Grammar11 (pGrammar11)
import Parser.Rules.Grammar12 (pGrammar12)
import Parser.Rules.Grammar13 (pGrammar13)
import Parser.Rules.Grammar14 (pGrammar14)
import Parser.Rules.Grammar15 (pGrammar15)
import Parser.Rules.Grammar16 (pGrammar16Da, pGrammar16Sa)
import Parser.Rules.Grammar17 (pGrammar17)
import Parser.Rules.Grammar18 (pGrammar18)
import Parser.Rules.Grammar19 (pGrammar19)
import Parser.Rules.Grammar20 (pGrammar20)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

{-
Tibetan spelling structure 1
On the basis of the Tibetan spelling grammar 4.1
-}

pStructure1 :: Parser Text
pStructure1 = do
    struct <- choice
        [ try pGrammar1WithLong
        , try pGrammar1Sanskrit
        ]
    eof -- TODO: update syllable ending
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "སཱ"
-- Right "སཱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ས"
-- Right "ས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "སོ"
-- Right "སོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ཊཱ"
-- Left "1:2:
--     |
--   1 | ཊཱ
--     |  ^
--   unexpected 'ཱ'
--   expecting Vowel character or end of input
--   "

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ཊོ"
-- Right "ཊོ"

{-
Tibetan spelling structure 2
On the basis of the Tibetan spelling grammar 4.8
-}

pStructure2 :: Parser Text
pStructure2 = do
    struct <- pGrammar8
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure2 "སྒ"
-- Right "སྒ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure2 "སྒ"
-- Right "སྒ"

{-
Tibetan spelling structure 3
On the basis of the Tibetan spelling grammar 4.9
-}

pStructure3 :: Parser Text
pStructure3 = do
    struct <- pGrammar9
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure3 "སྲོ"
-- Right "སྲོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure3 "སླ"
-- Right "སླ"

{-
Tibetan spelling structure 4
On the basis of the Tibetan spelling grammar 4.10
-}

pStructure4 :: Parser Text
pStructure4 = do
    struct <- pGrammar10
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "རྐྱུ"
-- Right "རྐྱུ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྐྲོ"
-- Right "སྐྲོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྒྲ"
-- Right "སྒྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྤྱ"
-- Right "སྤྱ"

{-
Tibetan spelling structure 5
On the basis of the Tibetan spelling grammar 4.11
-}

pStructure5 :: Parser Text
pStructure5 = do
    struct <- pGrammar11
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure5 "བལྟ"
-- Right "བལྟ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure5 "བརྒ"
-- Right "བརྒ"

{-
Tibetan spelling structure 6
On the basis of the Tibetan spelling grammar 4.12
-}

pStructure6 :: Parser Text
pStructure6 = do
    struct <- pGrammar12
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "འདྲ"
-- Right "འདྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "མགྱ"
-- Right "མགྱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "མགྲ"
-- Right "མགྲ"

{-
Tibetan spelling structure 7
On the basis of the Tibetan spelling grammar 4.13
-}

pStructure7 :: Parser Text
pStructure7 = do
    struct <- pGrammar13
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure7 "བརྒྱ"
-- Right "བརྒྱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure7 "བསྒྲ"
-- Right "བསྒྲ"

{-
Tibetan spelling structure 8
On the basis of the Tibetan spelling grammar 4.14
-}

pStructure8 :: Parser Text
pStructure8 = do
    struct <- pGrammar14
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure8 "བཏ"
-- Right "བཏ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure8 "གཏ"
-- Right "གཏ"

{-
Tibetan spelling structure 9
On the basis of the Tibetan spelling grammar 4.14 and 4.15
-}

pStructure9 :: Parser Text
pStructure9 = do
    struct <- pGrammar14
    suffix <- pGrammar15
    pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure9 "བཏག"
-- Right "བཏག"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure9 "བཏིག"
-- Right "བཏིག"

{-
Tibetan spelling structure 10
On the basis of the Tibetan spelling grammar 4.11 and 4.15
-}

pStructure10 :: Parser Text
pStructure10 = do
    structure8 <- pGrammar11
    suffix <- pGrammar15
    eof
    pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure10 "བརྒན"
-- Right "བརྒན"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure10 "བརྒིག"
-- Right "བརྒིག"

{-
Tibetan spelling structure 11
On the basis of the Tibetan spelling grammar 4.12 and 4.15
-}

pStructure11 :: Parser Text
pStructure11 = do
    structure8 <- pGrammar12
    suffix <- pGrammar15
    pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྱས"
-- Right "མགྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྲས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྲིས"
-- Right "མགྲིས"

{-
Tibetan spelling structure 12
On the basis of the Tibetan spelling grammar 4.13 and 4.15
-}

pStructure12 :: Parser Text
pStructure12 = do
    structure8 <- pGrammar13
    suffix <- pGrammar15
    pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བརྒྱས"
-- Right "བརྒྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྲས"
-- Right "བསྒྲས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྲོས"
-- Right "བསྒྲོས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྱས"
-- Right "བསྒྱས"

{-
Tibetan spelling structure 13
On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
-}

pStructure13 :: Parser Text
pStructure13 = do
    struct <-
        choice
            [ try $ parse13 pGrammar16Da pPostfixDa
            , try $ parse13 pGrammar16Sa pPostfixSa
            ]
    eof
    pure struct

parse13 :: Parser Char -> Parser Char -> Parser Text
parse13 parseSuffix parsePostfix = do
    struct <- pGrammar14
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure13 "བཏནད"
-- Right "བཏནད"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure13 "གཏིགས"
-- Right "གཏིགས"

{-
Tibetan spelling structure 14
On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
-}

pStructure14 :: Parser Text
pStructure14 = do
    struct <-
        choice
            [ try $ parse14 pGrammar16Da pPostfixDa
            , try $ parse14 pGrammar16Sa pPostfixSa
            ]
    eof
    pure struct

parse14 :: Parser Char -> Parser Char -> Parser Text
parse14 parseSuffix parsePostfix = do
    struct <- pGrammar11
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure14 "བརྟགས"
-- Right "བརྟགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure14 "བརྟིབས"
-- Right "བརྟིབས"

{-
Tibetan spelling structure 15
On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
-}

pStructure15 :: Parser Text
pStructure15 = do
    struct <-
        choice
            [ try $ parse15 pGrammar16Da pPostfixDa
            , try $ parse15 pGrammar16Sa pPostfixSa
            ]
    eof
    pure struct

parse15 :: Parser Char -> Parser Char -> Parser Text
parse15 parseSuffix parsePostfix = do
    struct <- pGrammar12
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure15 "དགྱིགས"
-- Right "དགྱིགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure15 "དགྲོགས"
-- Right "དགྲོགས"

{-
Tibetan spelling structure 16
On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
-}

pStructure16 :: Parser Text
pStructure16 = do
    struct <-
        choice
            [ try $ parse16 pGrammar16Da pPostfixDa
            , try $ parse16 pGrammar16Sa pPostfixSa
            ]
    eof
    pure struct

parse16 :: Parser Char -> Parser Char -> Parser Text
parse16 parseSuffix parsePostfix = do
    struct <- pGrammar13
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure16 "བསྒྱུགས"
-- Right "བསྒྱུགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure16 "བརྐྱནད"
-- Right "བརྐྱནད"

{-
Tibetan spelling structure 17
On the basis of the Tibetan spelling grammar 4.15
-}

pStructure17 :: Parser Text
pStructure17 = do
    root <- pRootConsonant
    vowel <- optional pVowel
    suffix <- pGrammar15
    eof
    let consT = T.empty :> root
    let consVowelT = maybe consT (consT :>) vowel
    pure $ consVowelT :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure17 "བིག"
-- Right "བིག"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure17 "བས"
-- Right "བས"

{-
Tibetan spelling structure 18
On the basis of the Tibetan spelling grammar 4.8 and 4.15
-}

pStructure18 :: Parser Text
pStructure18 = do
    struct <- pGrammar8
    suffix <- pGrammar15
    eof
    pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure18 "རྒིད"
-- Right "རྒིད"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure18 "སྒོལ"
-- Right "སྒོལ"

{-
Tibetan spelling structure 19
On the basis of the Tibetan spelling grammar 4.9 and 4.15
-}

pStructure19 :: Parser Text
pStructure19 = do
    struct <- pGrammar9
    suffix <- pGrammar15
    eof
    pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure19 "ཁྱོའ"
-- Right "ཁྱོའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure19 "བླིན"
-- Right "བླིན"

{-
Tibetan spelling structure 20
On the basis of the Tibetan spelling grammar 4.10 and 4.15
-}

pStructure20 :: Parser Text
pStructure20 = do
    struct <- pGrammar10
    suffix <- pGrammar15
    eof
    pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure20 "སྐྲོན"
-- Right "སྐྲོན"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure20 "སྒྱར"
-- Right "སྒྱར"

{-
Tibetan spelling structure 21
On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
-}

pStructure21 :: Parser Text
pStructure21 =
    choice
        [ try $ parse21 pGrammar16Da pPostfixDa
        , try $ parse21 pGrammar16Sa pPostfixSa
        ]

parse21 :: Parser Char -> Parser Char -> Parser Text
parse21 parseSuffix parsePostfix = do
    root <- pRootConsonant
    vowel <- optional pVowel
    suffix <- parseSuffix
    postfix <- parsePostfix
    eof
    let consT = T.empty :> root
    let consVowelT = maybe consT (consT :>) vowel
    pure $ consVowelT :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure21 "བགས"
-- Right "བགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure21 "པོགས"
-- Right "པོགས"

{-
Tibetan spelling structure 22
On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
-}

pStructure22 :: Parser Text
pStructure22 =
    choice
        [ try $ parse22 pGrammar16Da pPostfixDa
        , try $ parse22 pGrammar16Sa pPostfixSa
        ]

parse22 :: Parser Char -> Parser Char -> Parser Text
parse22 parseSuffix parsePostfix = do
    struct <- pGrammar8
    suffix <- parseSuffix
    postfix <- parsePostfix
    eof
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure22 "རྨགས"
-- Right "རྨགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure22 "སྣོབས"
-- Right "སྣོབས"

{-
Tibetan spelling structure 23
On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
-}

pStructure23 :: Parser Text
pStructure23 =
    choice
        [ try $ parse23 pGrammar16Da pPostfixDa
        , try $ parse23 pGrammar16Sa pPostfixSa
        ]

parse23 :: Parser Char -> Parser Char -> Parser Text
parse23 parseSuffix parsePostfix = do
    struct <- pGrammar9
    suffix <- parseSuffix
    postfix <- parsePostfix
    eof
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simpl
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure23 "རློམས"
-- Right "རློམས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure23 "དྭོལད"
-- Right "དྭོལད"

{-
Tibetan spelling structure 24
On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
-}

pStructure24 :: Parser Text
pStructure24 =
    choice
        [ try $ parse24 pGrammar16Da pPostfixDa
        , try $ parse24 pGrammar16Sa pPostfixSa
        ]

parse24 :: Parser Char -> Parser Char -> Parser Text
parse24 parseSuffix parsePostfix = do
    struct <- pGrammar10
    suffix <- parseSuffix
    postfix <- parsePostfix
    eof
    pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure24 "རྩྭོངས"
-- Right "རྩྭོངས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure24 "སྣྲེནད"
-- Right "སྣྲེནད"

{-
Tibetan spelling structure 25
On the basis of the Tibetan spelling grammar 4.17
-}

pStructure25 :: Parser Text
pStructure25 = do
    struct <- pGrammar17
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure25 "དྲྭ"
-- Right "དྲྭ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure25 "ཕྱྭ"
-- Right "ཕྱྭ"

{-
Tibetan spelling structure 26
On the basis of the Tibetan spelling grammar 4.18
-}

pStructure26 :: Parser Text
pStructure26 = do
    struct <- pGrammar18
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure26 "ཧྥ"
-- Right "ཧྥ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure26 "ཧྥོ"
-- Right "ཧྥོ"

{-
Tibetan spelling structure 27
On the basis of the Tibetan spelling grammar 4.19
-}

pStructure27 :: Parser Text
pStructure27 = do
    struct <- pGrammar19
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure27 "ཧྥེལ"
-- Right "ཧྥེལ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure27 "ཧྥོས"
-- Right "ཧྥོས"

{-
Tibetan spelling structure 28
On the basis of the Tibetan spelling grammar 4.1 and 4.20
-}

pStructure28 :: Parser Text
pStructure28 = do
    root <- pGrammar1
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure28 "ཧིའེ"
-- Right "ཧིའེ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure28 "གོའྨ"
-- Right "གོའྨ"

{-
Tibetan spelling structure 29
On the basis of the Tibetan spelling grammar 4.8 and 4.20
-}

pStructure29 :: Parser Text
pStructure29 = do
    root <- pGrammar8
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure29 "སྨོའྨ"
-- Right "སྨོའྨ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure29 "རྒའོ"
-- Right "རྒའོ"

{-
Tibetan spelling structure 30
On the basis of the Tibetan spelling grammar 4.9 and 4.20
-}

pStructure30 :: Parser Text
pStructure30 = do
    root <- pGrammar9
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure30 "སླིའྔ"
-- Right "སླིའྔ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure30 "ཀྭོའོ"
-- Right "ཀྭོའོ"

{-
Tibetan spelling structure 31
On the basis of the Tibetan spelling grammar 4.10 and 4.20
-}

pStructure31 :: Parser Text
pStructure31 = do
    root <- pGrammar10
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure31 "སྒྱའོ"
-- Right "སྒྱའོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure31 "རྐྱའོ"
-- Right "རྐྱའོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure31 "རྐྱའ"
-- Right "རྐྱའ"

{-
Tibetan spelling structure 32
On the basis of the Tibetan spelling grammar 4.11 and 4.20
-}

pStructure32 :: Parser Text
pStructure32 = do
    root <- pGrammar11
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure32 "བལྟའ"
-- Right "བལྟའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure32 "བརྔོའ"
-- Right "བརྔོའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure32 "བསྟིའི"
-- Right "བསྟིའ"

{-
Tibetan spelling structure 33
On the basis of the Tibetan spelling grammar 4.12 and 4.20
-}

pStructure33 :: Parser Text
pStructure33 = do
    root <- pGrammar12
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure33 "མཁྲའ"
-- Right "མཁྲའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure33 "དཔྱུའ"
-- Right "དཔྱུའ"

{-
Tibetan spelling structure 34
On the basis of the Tibetan spelling grammar 4.13 and 4.20
-}

pStructure34 :: Parser Text
pStructure34 = do
    root <- pGrammar13
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure34 "བསྐྲའ"
-- Right "བསྐྲའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure34 "བརྒྱའ"
-- Right "བརྒྱའ"

{-
Tibetan spelling structure 35
On the basis of the Tibetan spelling grammar 4.14 and 4.20
-}

pStructure35 :: Parser Text
pStructure35 = do
    root <- pGrammar14
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure35 "བཟིའ"
-- Right "བཟིའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure35 "གཤའ"
-- Right "གཤའ"

{-
Tibetan spelling structure 36
On the basis of the Tibetan spelling grammar 4.17 and 4.20
-}

pStructure36 :: Parser Text
pStructure36 = do
    root <- pGrammar17
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure36 "གྲྭའ"
-- Right "གྲྭའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure36 "ཕྱྭུའ"
-- Right "ཕྱྭུའ"

{-
Tibetan spelling structure 37
On the basis of the Tibetan spelling grammar 4.18 and 4.20
-}

pStructure37 :: Parser Text
pStructure37 = do
    root <- pGrammar18
    structA <- pGrammar20
    eof
    pure $ T.append root structA

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure37 "ཧྥོའ"
-- Right "ཧྥོའ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure37 "ཧྥའ"
-- Right "ཧྥའ"
