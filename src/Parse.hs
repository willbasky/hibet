module Parse
       ( toTibet
       , Tibet
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec.Parsers
import Data.RadixTree

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Error as ME


type Parser a = ParsecT Void Text Identity a
type ParseError = ME.ParseErrorBundle Text Void

parseT :: Parser a -> String -> Text -> Either ParseError a
parseT = M.runParser . unParsecT

-- parseTest :: Show a => Parser a -> Text -> IO ()
-- parseTest = M.parseTest . unParsecT

toTibet :: Text -> Wylie -> [Tibet]
toTibet syls = makeTibet (makeWylieTibet syls) . parsedRadex syls

radexTree :: Text -> (Text -> Either ParseError [Text])
radexTree syls =
    let linedSyls = T.lines syls
        firstPart = map (T.takeWhile (/= '|')) linedSyls
        radex = fromFoldable firstPart
    in  parseT (search radex) ""

makeTibet :: WylieTibet -> [([Wylie], [[Wylie]])] -> [Tibet]
makeTibet wt ts =
    let look = flip HMS.lookup wt
        fromLook = T.concat . fromMaybe ["errorT"] . traverse look
    in map (\(f,s) -> uncurry spaceBetween (fromLook f, T.concat $ map fromLook s)) ts

parsedInput :: Text -> [(Text, [Text])]
parsedInput txt = map (\(f,s) -> (f, fromRight ["error3"] $ parseT tibSentEndList "" s))
    $ fromRight [("error2","")] $ tibList
    $ fromRight ["error1"] $ parseT tibLines "" txt

parsedRadex :: Text -> Text -> [([Text], [[Text]])]
parsedRadex syls wylie = map
    (\(f,s)
        -> (fromRight ["errorR"] $ radexTree syls f
        , map (fromRight ["errorR"] . radexTree syls) s)
    ) $ parsedInput wylie


spaceBetween :: Text -> Text -> Text
spaceBetween a b = a <> " " <> b

symbolM :: M.Tokens Text -> Parser (M.Tokens Text)
symbolM = ML.symbol MC.space

parensMNoSpace :: Parser a -> Parser a
parensMNoSpace = M.between (symbolM "(") (symbolM ")")

parensM :: Parser a -> Parser a
parensM = M.between (char '(') (char ')')

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
tibBase = try tibBaseSub
    <|> try letterInParens
    <|> T.singleton <$> try tibParens
    <|> try digits

tibBaseSen :: Parser Text
tibBaseSen = try tibBaseSub
    <|> try letterInParensSen
    <|> T.singleton <$> try tibParens
    <|> try digits

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
    second <- many (try oneLine)
    pure $ if T.null first then second else first : second

-- > traverse (parseT tibSentences "") ["(sdf)sdf","1.df"]
-- Right [("","(sdf)sdf"),("1.","df")]
tibList :: [Text] -> Either ParseError [(Text, Text)]
tibList = traverse (parseT tibSentences "")

tibSentences :: Parser (Text, Text)
tibSentences = try tibSen <|> try tibSenD <|> try tibSenDT

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
tibSentEndList = some $ try tibSentEndFE <|> try tibSentEndE <|> try tibSentEnd

-- safeListCall :: Foldable t => (t a -> b) -> t a -> Maybe b
-- safeListCall f xs
--     | null xs = Nothing
--     | otherwise = Just $ f xs

syllableParserWT :: Parser (Text, Text)
syllableParserWT = do
    w <- some $ M.anySingleBut '|'
    _ <- char '|'
    t <- some M.anySingle
    pure (T.pack w, T.pack t)

-- syllableParserTW :: Parser (Text, Text)
-- syllableParserTW = do
--     w <- some $ M.anySingleBut '|'
--     _ <- char '|'
--     t <- some M.anySingle
--     pure (T.pack t, T.pack w)


type WylieTibet = HashMap Wylie Tibet
-- type TibetWylie = HashMap Tibet Wylie

type Wylie = Text
type Tibet = Text

makeWylieTibet :: Text -> WylieTibet
makeWylieTibet
    = HMS.fromList
    . either (error . ME.errorBundlePretty) id
    . traverse (parseT syllableParserWT "")
    . T.lines

-- makeTibetWylie :: Text -> TibetWylie
-- makeTibetWylie
--     = HMS.fromList
--     . either (error . ME.errorBundlePretty) id
--     . traverse (parseT syllableParserTW "")
--     . T.lines


-------------
-- Sandbox --
-------------

-- All possible wylie chars and symbols from syllables file.
-- %'()+-./0123456789:@ADHIMNRSTUWXY_abcdefghijklmnoprstuvwyz~нсᨵ“”

-- sortWords :: Text -> IO ()
-- sortWords tibText = do
--     syls <- T.readFile "./tibetan-syllables"
--     T.putStrLn $ T.intercalate "\n" $ toTibet syls tibText
