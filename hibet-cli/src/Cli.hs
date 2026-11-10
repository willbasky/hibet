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
import Install
    ( Installed (..)
    , defaultReleaseSource
    , hibetDataDir
    , installFromDir
    , installFromRelease
    )
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
import Data.Maybe (fromMaybe, listToMaybe)
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
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
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
    | DataInstall FilePath
    | DataStatus
    | DataUpdate (Maybe String)
    | Om
    | ShowOption Opt

-- | Commands parsed with @show@ command
data Opt = Names | Meta (Maybe Int)

-- | Whether a command needs the dictionaries loaded.
commandNeedsEnv :: Action -> Bool
commandNeedsEnv = \case
    Shell _ -> True
    ShowOption _ -> True
    DataInstall _ -> False
    DataStatus -> False
    DataUpdate _ -> False
    Om -> False

-- | Run a command that does not need the dictionaries.
runStatic :: Command -> IO ()
runStatic (Command mdir action) = case action of
    DataInstall source -> runDataInstall source
    DataStatus -> runDataStatus mdir
    DataUpdate msource -> runDataUpdate msource
    Om -> putColorDoc magenta NewLine om
    Shell _ -> pure ()
    ShowOption _ -> pure ()

-- | Run 'hibet' with cli command
runCommand :: MVar Env -> Action -> IO ()
runCommand mv action = do
    case action of
        -- App.prepareEnv already filtered the dictionaries by these ids
        Shell _ -> translator mv
        DataInstall _ -> pure ()
        DataStatus -> pure ()
        DataUpdate _ -> pure ()
        Om -> putColorDoc magenta NewLine om
        ShowOption opt -> runShow mv opt

runDataStatus :: Maybe FilePath -> IO ()
runDataStatus mdir = do
    resolved <- resolveDataDir mdir
    root <- either throwIO pure resolved
    mversion <- readManifestVersion (root </> "manifest.toml")
    TIO.putStrLn $ "data dir: " <> T.pack root
    TIO.putStrLn $
        "version: " <> maybe "unknown (manifest.toml is missing)" id mversion

runDataInstall :: FilePath -> IO ()
runDataInstall source = do
    target <- hibetDataDir
    installFromDir source target >>= reportInstalled "install"

runDataUpdate :: Maybe String -> IO ()
runDataUpdate msource = do
    target <- hibetDataDir
    installFromRelease (fromMaybe defaultReleaseSource msource) target >>= \case
        Left err -> reportFailure "update" err
        Right Nothing -> do
            mversion <-
                readManifestVersion (target </> "data" </> "manifest.toml")
            TIO.putStrLn $
                "data already up to date"
                    <> maybe "" (\v -> " (" <> v <> ")") mversion
        Right (Just installed) -> reportSuccess installed

reportInstalled :: String -> Either String Installed -> IO ()
reportInstalled verb = either (reportFailure verb) reportSuccess

reportFailure :: String -> String -> IO ()
reportFailure verb err = do
    TIO.hPutStrLn
        stderr
        ("data " <> T.pack verb <> " failed: " <> T.pack err)
    exitFailure

reportSuccess :: Installed -> IO ()
reportSuccess installed = do
    TIO.putStrLn $
        "installed data "
            <> installed.installedVersion
            <> " ("
            <> T.pack (show installed.installedFiles)
            <> " data files)"
    TIO.putStrLn $ "data dir: " <> T.pack installed.installedPath

-- | 'hibet data status' only needs the version; the full schema parser lives
-- in 'Manifest'.
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
                ( info
                    (helper <*> dataP)
                    $ progDesc "Show, install and update dictionary data"
                )

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
            <> command
                "install"
                ( info
                    (helper <*> installP)
                    $ progDesc "Install data from a directory into $XDG_DATA_HOME/hibet/data"
                )
            <> command
                "update"
                ( info
                    (helper <*> updateP)
                    $ progDesc "Download and install the latest data release"
                )

installP :: Parser Action
installP =
    DataInstall
        <$> strOption
            ( long "from"
                <> metavar "DIR"
                <> help "Data directory with manifest.toml, titles.toml and dicts/"
            )

updateP :: Parser Action
updateP =
    DataUpdate
        <$> optional
            ( strOption
                ( long "source"
                    <> metavar "URL|DIR"
                    <> help "Release base URL or directory (default: latest GitHub release)"
                )
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
