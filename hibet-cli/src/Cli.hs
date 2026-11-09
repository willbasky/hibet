{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli
    ( Command (..)
    , Action (..)
    , commandNeedsEnv
    , runCommand
    , runStatic
    , parser
    ) where

import Env (Env (..), readEnv, resolveDataDir)
import Label (LabelFull (..), Labels (..), Title (unTitle))
import Paths_hibet_cli (version)
import Pretty
import Shell (translator)

import Utility (showT)

import Control.Applicative (many, optional, (<|>))
import Control.Concurrent.MVar (MVar)
import Control.Exception (throwIO)
import Data.Foldable (find, toList)
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import NeatInterpolation (text)
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , command
    , fullDesc
    , help
    , helper
    , info
    , infoHeader
    , infoOption
    , long
    , metavar
    , option
    , progDesc
    , short
    , strOption
    , subparser
    )
import Options.Applicative.Help.Chunk (stringChunk)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Prelude hiding (lookup)

---------------------------------------------------------------------------
-- CLI
---------------------------------------------------------------------------

-- | Represent all available commands
data Command = Command
    { dataDir :: !(Maybe FilePath)
    , action :: !Action
    }

data Action
    = -- | @shell@ command launch translating shell
      Shell [Int]
    | DataStatus
    | Om
    | ShowOption Opt

-- | Commands parsed with @show@ command
data Opt = Names | Meta (Maybe Int)

-- | Whether a command needs the dictionaries loaded.
commandNeedsEnv :: Action -> Bool
commandNeedsEnv = \case
    Shell _ -> True
    ShowOption _ -> True
    DataStatus -> False
    Om -> False

-- | Run a command that does not need the dictionaries.
runStatic :: Command -> IO ()
runStatic (Command mdir action) = case action of
    DataStatus -> runDataStatus mdir
    Om -> putColorDoc magenta NewLine om
    Shell _ -> pure ()
    ShowOption _ -> pure ()

-- | Run 'hibet' with cli command
runCommand :: MVar Env -> Action -> IO ()
runCommand mv action = do
    case action of
        -- App.prepareEnv already filtered the dictionaries by these ids
        Shell _ -> translator mv
        Om -> putColorDoc magenta NewLine om
        ShowOption opt -> runShow mv opt
        DataStatus -> pure ()

runDataStatus :: Maybe FilePath -> IO ()
runDataStatus mdir = do
    resolved <- resolveDataDir mdir
    root <- either throwIO pure resolved
    mversion <- readManifestVersion (root </> "manifest.toml")
    TIO.putStrLn $ "data dir: " <> T.pack root
    TIO.putStrLn $
        "version: " <> maybe "unknown (manifest.toml is missing)" id mversion

-- | The manifest is a flat TOML file; 'hibet data status' only needs its
-- version, so the full schema parser lives with the updater.
readManifestVersion :: FilePath -> IO (Maybe Text)
readManifestVersion path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            content <- TIO.readFile path
            pure $
                listToMaybe
                    [ T.dropAround (== '"') $ T.strip (T.drop 1 rest)
                    | line <- T.lines content
                    , let (key, rest) = T.breakOn "=" line
                    , T.strip key == "version"
                    , not (T.null rest)
                    ]

runShow :: MVar Env -> Opt -> IO ()
runShow mv opt = do
    env <- readEnv mv
    let Labels labels = env.labels
    let filteredLabels = filterAvailable labels
    case opt of
        Names -> do
            let titles = sortById filteredLabels
            mapM_
                ( \label -> do
                    putColorList
                        [ (cyan, showT label.lfId <> ". ")
                        , (green, unTitle label.label <> ". ")
                        , (cyan, maybe "" (const "Year ") label.year)
                        , (green, maybe "" (flip T.append ". " . showT) label.year)
                        , (cyan, "From ")
                        , (green, label.source <> " ")
                        , (cyan, "to ")
                        , (green, T.intercalate ", " (toList label.target) <> ".")
                        ]
                    putColorDoc blue NewLine ""
                )
                titles
            putColorDoc yellow NewLine
                $ T.pack
                $ "Available dictionaries: " <> show (length titles)
        Meta Nothing -> do
            mapM_
                ( \label -> do
                    putColorList
                        [ (cyan, showT label.lfId <> ". ")
                        , (green, unTitle label.label)
                        , (cyan, maybe "" (const ". Year ") label.year)
                        , (green, maybe "" showT label.year)
                        ]
                    putColorDoc blue NewLine ""
                    putColorDoc blue NewLine label.about
                    putColorList
                        [ (cyan, "From ")
                        , (green, label.source <> " ")
                        , (cyan, "to ")
                        , (green, T.intercalate ", " (toList label.target))
                        ]
                    putColorDoc blue NewLine ""
                )
                filteredLabels
            putColorDoc yellow NewLine
                $ T.pack
                $ "Available dictionaries: " <> show (length filteredLabels)
        Meta (Just n) -> do
            case find (\label -> n == label.lfId) filteredLabels of
                Nothing ->
                    putColorDoc
                        red
                        NewLine
                        "No such dictionary number! Run 'hibet show names' to list ids."
                Just label -> do
                    putColorDoc green NewLine $
                        showT label.lfId
                            <> ". "
                            <> unTitle label.label
                    putColorDoc blue NewLine label.about
    where
        sortById :: [LabelFull] -> [LabelFull]
        sortById =
            sortBy
                ( \labelFull1 labelFull2 ->
                    compare labelFull1.lfId labelFull2.lfId
                )
        filterAvailable :: [LabelFull] -> [LabelFull]
        filterAvailable = filter available

----------------------------------------------------------------------------
-- Command parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
parser :: ParserInfo Command
parser =
    modifyHeader $
        info (helper <*> versionP <*> commandP) fullDesc

commandP :: Parser Command
commandP =
    Command
        <$> optional dataDirP
        <*> (shellP <|> commands)

versionP :: Parser (a -> a)
versionP =
    infoOption tibetVersion $
        long "version"
            <> short 'v'
            <> help "Show Hibet's version"

tibetVersion :: String
tibetVersion =
    T.unpack
        $ T.intercalate "\n"
        $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
    where
        sVersion = textToColorText green $ "Hibet " <> "v" <> T.pack (showVersion version)
        sHash = " ➤ " <> (textToColorText blue "Git revision: " <> $(gitHash))
        sDate = " ➤ " <> (textToColorText blue "Commit date:  " <> $(gitCommitDate))
        sDirty = textToColorText red "There are non-committed files."

-- All possible commands.
commands :: Parser Action
commands =
    subparser $
        command
            "shell"
            (info (helper <*> shellP) $ progDesc "Start the translate shell")
            <> command "om" (info (helper <*> pure Om) $ progDesc "Print Om to a terminal")
            <> command
                "show"
                (info (helper <*> showP) $ progDesc "Show names or meta of dictionaries")
            <> command
                "data"
                (info (helper <*> dataP) $ progDesc "Show dictionary data information")

shellP :: Parser Action
shellP = Shell <$> idListP

dataDirP :: Parser FilePath
dataDirP =
    strOption $
        long "data-dir"
            <> metavar "DIR"
            <> help "Directory with titles.toml, key-aliases.tsv and dicts/"

dataP :: Parser Action
dataP =
    subparser $
        command
            "status"
            ( info
                (helper <*> pure DataStatus)
                $ progDesc "Show the resolved data directory and its version"
            )

idListP :: Parser [Int]
idListP =
    many
        $ option auto
        $ long "select"
            <> short 's'
            <> help "Select id of dictionary (see 'hibet show names')"
            <> metavar "DICT_ID"

showP :: Parser Action
showP =
    ShowOption
        <$> subparser
            ( command
                "names"
                (info (helper <*> pure Names) $ progDesc "Show dictionary titles")
                <> command
                    "meta"
                    (info (helper <*> dictNumber) $ progDesc "Show dictionary descriptions")
            )

dictNumber :: Parser Opt
dictNumber =
    Meta
        <$> optional
            ( option
                auto
                ( long "dictionary"
                    <> short 'd'
                    <> metavar "DICTIONARY_NUMBER"
                    <> help "Show specific dictionary description (see 'hibet show names')"
                )
            )

----------------------------------------------------------------------------
-- Beauty util
----------------------------------------------------------------------------

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p{infoHeader = stringChunk $ T.unpack artHeader}

artHeader :: Text
artHeader =
    textToColorText
        yellow
        "Hibet is command line translator from Tibet to English language."

endLine :: Text
endLine = "\n"

om :: Text
om =
    [text|
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
