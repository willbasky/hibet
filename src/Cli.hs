{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli
       ( trans
       ) where

import Data.Foldable (find)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, ReadM, auto, command, eitherReader, execParser,
                            fullDesc, help, helper, info, infoHeader, infoOption, long, metavar,
                            option, optional, progDesc, short, subparser, (<|>))
import Options.Applicative.Help.Chunk (stringChunk)

import Labels (LabelFull (..), labels)
import Paths_tibet (version)
import Prettify (blue, bold, endLine, green, magenta, putTextFlush, red, resetCode, yellow)
import Tibet (start)
import Data.Void (Void)

import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL


----------------------------------------------------------------------------
-- Command data types
----------------------------------------------------------------------------

-- | Represent all available commands
data Command
    -- | @shell@ command launch translating shell
    = Shell Select
    | Om
    | ShowOption Opt

-- | Commands parsed with @show@ command
data Opt = Names | Meta (Maybe Int)

type Select = Maybe [Int]

---------------------------------------------------------------------------
-- CLI
---------------------------------------------------------------------------

trans :: IO ()
trans = execParser prsr >>= runCommand

-- | Run 'tibet' with cli command
runCommand :: Command -> IO ()
runCommand = \case
    Shell select -> start select
    Om -> putTextFlush $ magenta om
    ShowOption opt -> runShow opt

runShow :: Opt -> IO ()
runShow = \case
    Names -> do
        titles <- sortLabels <$> labels
        mapM_ (\LabelFull{..} -> putTextFlush $ green $ number lfId <> blue lfLabel)
            $ titles
    Meta Nothing -> do
        titles <- labels
        mapM_ (\LabelFull{..} -> do
            putTextFlush $ green $ number lfId <> green lfLabel
            putTextFlush $ blue lfMeta
            ) titles
    Meta (Just n) -> find (\LabelFull{..} -> n == lfId) <$> labels >>= \case
        Nothing -> putTextFlush $ red "No such number of dictionary!"
        Just LabelFull{..} -> do
            putTextFlush $ green $ number lfId <> green lfLabel
            putTextFlush $ blue lfMeta
  where
    number :: Int -> Text
    number n = green . T.pack $ show n <> ". "
    sortLabels :: [LabelFull] -> [LabelFull]
    sortLabels = sortBy (\(LabelFull _ a _ _) (LabelFull _ b _ _) -> compare a b)

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
prsr :: ParserInfo Command
prsr = modifyHeader
    $ info (helper <*> versionP <*> (shellP <|> commands))
    $ fullDesc
   <> progDesc "Translate from Tibetan to English"

versionP :: Parser (a -> a)
versionP = infoOption (T.unpack tibetCliVersion)
    $ long "version"
   <> short 'v'
   <> help "Show TibetCli's version"

tibetCliVersion :: Text
tibetCliVersion = T.intercalate "\n" $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = blue . bold $ "TibetCli " <> "v" <> T.pack (showVersion version)
    sHash = " ➤ " <> (blue . bold $ "Git revision: " <> resetCode <> $(gitHash))
    sDate = " ➤ " <> (blue . bold $ "Commit date:  " <> resetCode <> $(gitCommitDate))
    sDirty = red "There are non-committed files."

-- All possible commands.
commands :: Parser Command
commands = subparser
    $ command "shell" (info (helper <*> shellP) $ progDesc "Start the translate shell")
   <> command "om" (info (helper <*> pure Om) $ progDesc "Print Om to a terminal")
   <> command "show" (info (helper <*> showP) $ progDesc "Show titles or descriptions of dictionaries")

shellP :: Parser Command
shellP = Shell <$> optional idListP

idListP :: Parser [Int]
idListP = option attoparsecIdReader
    $ long "select"
   <> short 's'
   <> help "Select id list of dictionaries separeted by space or comma"
   <> metavar "ID_LIST"

showP :: Parser Command
showP = ShowOption <$> subparser
    ( command "names" (info (helper <*> pure Names) $ progDesc "Show dictionary titles")
   <> command "meta" (info (helper <*> dictNumber) $ progDesc "Show dictionary descriptions")
    )

dictNumber :: Parser Opt
dictNumber = Meta <$> optional
    (option auto
        (  long "dictionary"
        <> short 'd'
        <> metavar "DICTIONARY_NUMBER"
        <> help "Show specific dictionary description"
        ))

----------------------------------------------------------------------------
-- Beauty util
----------------------------------------------------------------------------

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p {infoHeader = stringChunk $ T.unpack artHeader}

artHeader :: Text
artHeader = yellow "TibetCli is command line translator from Tibet to English language."

om :: Text
om = [text|
$endLine
                             /hd+
                           /Nd::ohs.
                          /MM- .NMm+
                          :MMdosMMN.        '.-.
                  'oNNmdyo:/dMMMms/ohmNNNNNmy:'
                 'hMMMMMNmNN+.' .dmhyyso+/.
                 '.' '...:+ohy:.+.
                              .-     '.
              :yddddo '/mmNNmmdddmNNNm-  .mhhd-
             -mMMMMN. yMMMMMMMMMMMMMm+  +NMMMMo
             dMMMMM/  .+smMMMMMMMMd:.  :NMMMMMs
             'mMMMh    .hMMMs:hMMMM/    .mMMMMo
              +MMM/   -NNh/'   :mMMN:    .mMMMo
              'NMm    ms''-.'   'oNMN-    :MMM+
               mMy   -o+dNMMMNh:  .hMN-    hMM/
               yMy   .shydMMMMMM+   /Nm.   :MM:
               :Mh    '   :NMMMMm    -mm-  'mM'
                dN.        yMMMMo      sN-  sm
                'ym.      :NMMMo'       /d: /d
                  -yo:'.:yMMdo.          'yo.s
                     .--:--'               +y+
                                            :+
            |]

-- Customized error message.
attoparsecIdReader :: ReadM [Int]
attoparsecIdReader = eitherReader $ \select ->
    either (\err -> Left (select <> " could not be parsed. An error has occured: " <> show err))
        Right (M.parse numListParserM "" select)

-- Parse list of numbers. Possible to parse: 1,2,3 or "1,2 3" or "1 2 3" or "1,2,3"
numListParserM :: M.Parsec Void String [Int]
numListParserM = MCL.decimal `M.sepBy1` M.choice [MC.char ',', MC.char ' ']

