module Parse.Script where


import Parse.Type
import Type (HibetError (..))

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad.Except (Except)
import qualified Data.Bimap as Bi
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
    d <- parensMNoSpace (some tibTextMP)
    -- It gives (bla ). With space that converted to tibetan dot
    pure $ T.concat ["(", T.pack d, ")"]
    -- It gives (bla).  Without space.
    -- pure $ T.concat ["(", T.pack d, " ", ")"]

letterInParensSen :: Parser Text
letterInParensSen = do
    d <- parensMNoSpace (some tibTextMP)
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
    b <- T.concat <$> some tibBase
    pure $ d <> b

tibBase :: Parser Text
tibBase = M.try tibBaseSub
    <|> M.try letterInParens
    <|> T.singleton <$> M.try tibParens
    <|> M.try digits

{-
parseTest tibBaseSen "(sdf)sdf"
"(sdf)"
parseTest tibBaseSen "(sdf)"
"(sdf)"
-}
tibBaseSen :: Parser Text
tibBaseSen = M.try tibBaseSub
    <|> M.try letterInParensSen
    <|> T.singleton <$> M.try tibParens
    <|> M.try digits

tibBaseSub :: Parser Text
tibBaseSub = do
    t <- some tibTextMP
    pure $ T.pack t

-- > parseTest tibLines "(sdf)sdf(1)sd1.sdf"
-- ["(sdf)sdf","(1)sd","1.sdf"]

-- > parseT tibLines "" "(sdf)sdf(1)sd1.sdf"
-- Right ["(sdf)sdf","(1)sd","1.sdf"]
tibLines :: Parser [Text]
tibLines = do
    first <- T.concat <$> many tibBase
    second <- many (M.try oneLine)
    pure $ if T.null first then second else first : second

-- > traverse (parseT tibSentences "") ["(sdf)sdf","1.df"]
-- Right [("","(sdf)sdf"),("1.","df")]
tibList :: [Text] -> Except HibetError [(Text, Text)]
tibList = traverse (parseT tibSentences "")

tibSentences :: Parser (Text, Text)
tibSentences = M.try tibSen <|> M.try tibSenD <|> M.try tibSenDT

tibSenDT :: Parser (Text,Text)
tibSenDT = do
    d <- chooseDigit
    sens <- T.concat <$> some tibBaseSen
    pure (d, sens)

tibSenD :: Parser (Text,Text)
tibSenD = do
    d <- chooseDigit <* M.eof
    pure (d, "")

tibSen :: Parser (Text,Text)
tibSen = do
    sens <- T.concat <$> some tibBaseSen
    pure ("", sens)

tibTextMP :: Parser Char
tibTextMP = tibEnd <|> MC.letterChar <|> MC.spaceChar <|> tibChars

tibTextMPNoEnd :: Parser Char
tibTextMPNoEnd = MC.letterChar <|> MC.spaceChar <|> tibChars <|> tibParens

tibChars :: Parser Char
tibChars
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

tibEnd :: Parser Char
tibEnd = MC.char '/'

tibEndSpace :: Parser Text
tibEndSpace = symbolM "/"

tibParens :: Parser Char
tibParens = MC.char '(' <* M.notFollowedBy afterParen <|> MC.char ')'

afterParen :: Parser Char
afterParen = do
    _ <- ML.decimal :: Parser Int
    MC.char ')'

tibSentEnd :: Parser Text
tibSentEnd = do
    t <- T.pack <$> some tibTextMPNoEnd <* M.eof
    pure $ T.concat [T.stripEnd t]

tibSentEndE :: Parser Text
tibSentEndE = do
    MC.space
    t <- T.pack <$> some tibTextMPNoEnd
    e2 <- tibEndSpace
    pure $ T.concat [T.stripEnd t, e2]

tibSentEndFE :: Parser Text
tibSentEndFE = do
    MC.space
    e1 <- tibEndSpace
    t <- T.pack <$> some tibTextMPNoEnd
    e2 <- tibEndSpace
    pure $ T.concat [e1, T.stripEnd t, e2]

-- > parseTest tibSentEndList "sdf / /sdf  / fgdg / /sdf/"
-- ["sdf/","/sdf/","fgdg/","/sdf/"]
tibSentEndList :: Parser [Text]
tibSentEndList = some $ M.try tibSentEndFE <|> M.try tibSentEndE <|> M.try tibSentEnd <|> M.try tibBase

-- safeListCall :: Foldable t => (t a -> b) -> t a -> Maybe b
-- safeListCall f xs
--     | null xs = Nothing
--     | otherwise = Just $ f xs


---------------------------------------------------------------------
-- Parse tibetan script
---------------------------------------------------------------------

tibetanDotM :: Parser (M.Tokens Text)
tibetanDotM = symbolM "་"

tibetanEnd :: Parser Text
tibetanEnd = symbolM "།"

tibetanWord :: Parser Text
tibetanWord = T.pack <$> some MC.alphaNumChar

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
-- Bimap from syllables
---------------------------------------------------------------------

makeBi :: [(WylieSyllable,TibetSyllable)] -> Bi.Bimap WylieSyllable TibetSyllable
makeBi = Bi.fromList

splitSyllables :: Text -> Except HibetError [(WylieSyllable,TibetSyllable)]
splitSyllables
    = traverse (parseT syllableParserWT "")
    . T.lines

syllableParserWT :: Parser (WylieSyllable,TibetSyllable)
syllableParserWT = do
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
