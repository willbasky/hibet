{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli
       ( trans
       ) where

import Control.Applicative (many, optional, (<|>))
import Data.Foldable (find, toList, traverse_)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, auto, command, execParser, fullDesc, help, helper,
                            info, infoHeader, infoOption, long, metavar, option, progDesc, short,
                            subparser)
import Options.Applicative.Help.Chunk (stringChunk)

import App (app)
import Hibet.Interpretator
import Hibet.Language
import Labels (LabelFull (..), labels)
import Paths_hibet (version)
import Pretty
import Types

import qualified Data.Text as T


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

type Select = [Int]

---------------------------------------------------------------------------
-- CLI
---------------------------------------------------------------------------

trans :: IO ()
trans = execParser prsr >>= runCommand

-- | Run 'tibet' with cli command
runCommand :: Command -> IO ()
runCommand = \case
    Shell selectedIds -> app selectedIds
    Om -> runHibet $ putColorTextH magenta NewLine om
    ShowOption opt -> runHibet $ runShow opt

runShow :: Opt -> Hibet ()
runShow opt =
  case opt of
    Names -> do
        titles <- sortById . filterAvailable <$> labels
        mapM_ (\LabelFull{..} -> do
          putColorList
            [ (cyan, toText lfId <> ". ")
            , (green, lfLabel <> ". ")
            , (cyan, maybe "" (const "Year ") lfYear)
            , (green, maybe "" (flip T.append ". " . toText) lfYear)
            , (cyan, "From ")
            , (green, lfSource <> " ")
            , (cyan, "to ")
            , (green, T.intercalate ", " (toList lfTarget) <> ".")]
          putColorTextH blue NewLine ""
          ) titles
        putColorTextH yellow NewLine $ T.pack $ "Available dictionaries: " <> show (length titles)
    Meta Nothing -> do
        titles <- filterAvailable <$> labels
        mapM_ (\LabelFull{..} -> do
            putColorList
              [ (cyan, toText lfId <> ". ")
              , (green, lfLabel)
              , (cyan, maybe "" (const ". Year ") lfYear)
              , (green, maybe "" toText lfYear)]
            putColorTextH blue NewLine ""
            putColorTextH blue NewLine lfAbout
            putColorList
              [ (cyan, "From ")
              , (green, lfSource <> " ")
              , (cyan, "to ")
              , (green, T.intercalate ", " (toList lfTarget))]
            putColorTextH blue NewLine ""
            ) titles
        putColorTextH yellow NewLine $ T.pack $ "Available dictionaries: " <> show (length titles)
    Meta (Just n) -> do
        availableLabels <- filterAvailable <$> labels
        case find (\LabelFull{..} -> n == lfId) availableLabels of
            Nothing -> putColorTextH red NewLine "No such number of dictionary!"
            Just LabelFull{..} -> do
              putColorTextH green NewLine $ toText lfId <> lfLabel
              putColorTextH blue NewLine lfAbout
  where
    toText :: Int -> Text
    toText n = T.pack $ show n
    sortById :: [LabelFull] -> [LabelFull]
    sortById = sortBy (\labelFull1 labelFull2 ->
        compare (lfId labelFull1) (lfId labelFull2))
    filterAvailable :: [LabelFull] -> [LabelFull]
    filterAvailable = filter lfAvailable
    putColorList = traverse_ (\(c,d) -> putColorTextH c CurrentLine d)

----------------------------------------------------------------------------
-- Command parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
prsr :: ParserInfo Command
prsr = modifyHeader
    $ info (helper <*> versionP <*> (shellP <|> commands)) fullDesc

versionP :: Parser (a -> a)
versionP = infoOption tibetVersion
    $ long "version"
   <> short 'v'
   <> help "Show Hibet's version"

tibetVersion :: String
tibetVersion = T.unpack $ T.intercalate "\n" $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = textToColorText green $ "Hibet " <> "v" <> T.pack (showVersion version)
    sHash = " ➤ " <> (textToColorText blue "Git revision: " <> $(gitHash))
    sDate = " ➤ " <> (textToColorText blue "Commit date:  " <> $(gitCommitDate))
    sDirty = textToColorText red "There are non-committed files."

-- All possible commands.
commands :: Parser Command
commands = subparser
    $ command "shell" (info (helper <*> shellP) $ progDesc "Start the translate shell")
   <> command "om" (info (helper <*> pure Om) $ progDesc "Print Om to a terminal")
   <> command "show" (info (helper <*> showP) $ progDesc "Show titles or descriptions of dictionaries")

shellP :: Parser Command
shellP = Shell <$> idListP

idListP :: Parser [Int]
idListP = many $ option auto
    $ long "select"
   <> short 's'
   <> help "Select id of dictionary"
   <> metavar "DICT_ID"

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
artHeader = textToColorText yellow "Hibet is command line translator from Tibet to English language."

endLine :: Text
endLine = "\n"

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
