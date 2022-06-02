module Parse.Script where


import Parse.Type
import Type (HibetError (..))

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad.Except (Except)
import Data.Char (isMark)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML


---------------------------------------------------------------------
-- Parse wylie script
---------------------------------------------------------------------

symbolM :: M.Tokens Text -> Parser (M.Tokens Text)
symbolM = ML.symbol MC.space

parensMNoSpace :: Parser a -> Parser a
parensMNoSpace = M.between (symbolM "(") (symbolM ")")

parensM :: Parser a -> Parser a
parensM = M.between (MC.char '(') (MC.char ')')

dotM :: Parser (M.Tokens Text)
dotM = symbolM "."

digitInParens :: Parser Text
digitInParens = do
    d <- parensM ML.decimal :: Parser Int
    pure $ T.concat ["(", T.pack $ show d, ")"]

letterInParens :: Parser Text
letterInParens = do
    d <- parensMNoSpace (some walTextMP)
    -- It gives (bla ). With space that converted to tibetan dot
    pure $ T.concat ["(", T.pack d, ")"]
    -- It gives (bla).  Without space.
    -- pure $ T.concat ["(", T.pack d, " ", ")"]

letterInParensSen :: Parser Text
letterInParensSen = do
    d <- parensMNoSpace (some walTextMP)
    pure $ T.concat ["(", T.pack d, ")"]

digitAndDot :: Parser Text
digitAndDot = do
    di <- ML.decimal
    d <- dotM
    pure $ T.concat [T.pack $ show (di :: Integer), d]

digits :: Parser Text
digits = do
    di <- ML.decimal <* M.notFollowedBy (MC.char '.' <|> MC.char ')')
    pure $ T.concat [T.pack $ show (di :: Integer)]

chooseDigit :: Parser Text
chooseDigit = digitInParens <|> digitAndDot

oneLine :: Parser Text
oneLine = do
    d <- chooseDigit
    b <- T.concat <$> some walBase
    pure $ d <> b

walBase :: Parser Text
walBase = M.try walBaseSub
    <|> M.try letterInParens
    <|> T.singleton <$> M.try walParens
    <|> M.try digits

{-
parseTest walBaseSen "(sdf)sdf"
"(sdf)"
parseTest walBaseSen "(sdf)"
"(sdf)"
-}
walBaseSen :: Parser Text
walBaseSen = M.try walBaseSub
    <|> M.try letterInParensSen
    <|> T.singleton <$> M.try walParens
    <|> M.try digits

walBaseSub :: Parser Text
walBaseSub = do
    t <- some walTextMP
    pure $ T.pack t

-- > parseTest walLines "(sdf)sdf(1)sd1.sdf"
-- ["(sdf)sdf","(1)sd","1.sdf"]

-- > parseT walLines "" "(sdf)sdf(1)sd1.sdf"
-- Right ["(sdf)sdf","(1)sd","1.sdf"]
walLines :: Parser [Text]
walLines = do
    first <- T.concat <$> many walBase
    second <- many (M.try oneLine)
    pure $ if T.null first then second else first : second

-- > traverse (parseT walSentences "") ["(sdf)sdf","1.df"]
-- Right [("","(sdf)sdf"),("1.","df")]
walList :: [Text] -> Except HibetError [(Text, Text)]
walList = traverse (parseT walSentences "")

walSentences :: Parser (Text, Text)
walSentences = M.try walSen <|> M.try walSenD <|> M.try walSenDT

walSenDT :: Parser (Text,Text)
walSenDT = do
    d <- chooseDigit
    sens <- T.concat <$> some walBaseSen
    pure (d, sens)

walSenD :: Parser (Text,Text)
walSenD = do
    d <- chooseDigit <* M.eof
    pure (d, "")

walSen :: Parser (Text,Text)
walSen = do
    sens <- T.concat <$> some walBaseSen
    pure ("", sens)

walTextMP :: Parser Char
walTextMP = walEnd <|> MC.letterChar <|> MC.spaceChar <|> walChars

walTextMPNoEnd :: Parser Char
walTextMPNoEnd = MC.letterChar <|> MC.spaceChar <|> walChars <|> walParens

walChars :: Parser Char
walChars
    =   MC.char '+'
    <|> MC.char '\''
    <|> MC.char ':'
    <|> MC.char '-'
    <|> MC.char '.'
    <|> MC.char '%'
    <|> MC.char '_'
    <|> MC.char '”'
    <|> MC.char '“'
    <|> MC.char '@'
    <|> MC.char '~'

walEnd :: Parser Char
walEnd = MC.char '/'

walEndSpace :: Parser Text
walEndSpace = symbolM "/"

walParens :: Parser Char
walParens = MC.char '(' <* M.notFollowedBy afterParen <|> MC.char ')'

afterParen :: Parser Char
afterParen = do
    _ <- ML.decimal :: Parser Int
    MC.char ')'

walSentEnd :: Parser Text
walSentEnd = do
    t <- T.pack <$> some walTextMPNoEnd <* M.eof
    pure $ T.concat [T.stripEnd t]

walSentEndE :: Parser Text
walSentEndE = do
    MC.space
    t <- T.pack <$> some walTextMPNoEnd
    e2 <- walEndSpace
    pure $ T.concat [T.stripEnd t, e2]

walSentEndFE :: Parser Text
walSentEndFE = do
    MC.space
    e1 <- walEndSpace
    t <- T.pack <$> some walTextMPNoEnd
    e2 <- walEndSpace
    pure $ T.concat [e1, T.stripEnd t, e2]

-- > parseTest walSentEndList "sdf / /sdf  / fgdg / /sdf/"
-- ["sdf/","/sdf/","fgdg/","/sdf/"]
walSentEndList :: Parser [Text]
walSentEndList = some $ M.try walSentEndFE <|> M.try walSentEndE <|> M.try walSentEnd <|> M.try walBase

-- safeListCall :: Foldable t => (t a -> b) -> t a -> Maybe b
-- safeListCall f xs
--     | null xs = Nothing
--     | otherwise = Just $ f xs


---------------------------------------------------------------------
-- Parse tibetan script
---------------------------------------------------------------------

tibetanDotM :: Parser Text
tibetanDotM = symbolM "་"

tibetanEnd :: Parser Text
tibetanEnd = symbolM "།"

tibetanMarks :: Parser Char
tibetanMarks = M.satisfy isMark

tibetanWord :: Parser Text
tibetanWord =  T.pack <$> some (MC.alphaNumChar <|> tibetanMarks)

tibetanScriptEnd :: Parser Text
tibetanScriptEnd = tibetanWord <* tibetanEnd

tibetanScriptDot :: Parser Text
tibetanScriptDot = tibetanWord <* tibetanDotM

tibetanScriptDots :: Parser [Text]
tibetanScriptDots = some (M.try tibetanScriptDot)

tibetanScriptDotsNoDot :: Parser [Text]
tibetanScriptDotsNoDot = do
    tds <- tibetanScriptDots
    t <- M.try tibetanWord
    pure $ tds <> [t]

tibetanScriptDotsEnd :: Parser [Text]
tibetanScriptDotsEnd = do
    tds <- tibetanScriptDots
    te <- M.try tibetanScriptEnd
    pure $ tds <> [te]

-- | Parse tibetan script. Order matters.
--
{--
*Parse> parseTest tibetanScript  "མ་མ་མ"
"མ་མ་མ"
*Parse> parseTest tibetanScript  "མ་མ་མ་"
"མ་མ་མ"
*Parse> parseTest tibetanScript  "མ་མ་མ།"
"མ་མ་མ"
*Parse> parseTest tibetanScript  "མ་"
"མ"
*Parse> parseTest tibetanScript  "མ།"
"མ"
*Parse> parseTest tibetanScript  "མ"
"མ"
--}
tibetanScript :: Parser [Text]
tibetanScript = (:[]) <$> M.try tibetanScriptEnd
    <|> M.try tibetanScriptDotsEnd
    <|> M.try tibetanScriptDotsNoDot
    <|> M.try tibetanScriptDots
    <|> (:[]) <$> M.try tibetanWord


---------------------------------------------------------------------
-- Hashmaps from syllables
---------------------------------------------------------------------

splitSyllables :: Text -> Except HibetError [(WylieSyllable,TibetSyllable)]
splitSyllables
    = traverse (parseT parseSyllables "")
    . T.lines

parseSyllables :: Parser (WylieSyllable,TibetSyllable)
parseSyllables = do
    w <- some $ M.anySingleBut '|'
    _ <- MC.char '|'
    t <- some M.anySingle
    pure (WylieSyllable $ T.pack w, TibetSyllable $ T.pack t)

-------------
-- Sandbox --
-------------

-- All possible wylie chars and symbols from syllables file.
-- %'()+-./0123456789:@ADHIMNRSTUWXY_abcdefghijklmnoprstuvwyz~нсᨵ“”

-- sortWords :: Text -> IO ()
-- sortWords tibText = do
--     syls <- T.readFile "./tibetan-syllables"
--     T.putStrLn $ T.intercalate "\n" $ toTibet syls tibText
