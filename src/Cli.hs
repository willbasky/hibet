{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli
       ( runCommand
       , parser
       ) where

import Dictionary (selectDict)
import Effects.Console
import Effects.PrettyPrint
import Paths_hibet (version)
import Pretty
import Translator (translator)
import Types

import Control.Applicative (many, optional, (<|>))
import Data.Foldable (find, toList)
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, auto, command, fullDesc, help, helper, info,
                            infoHeader, infoOption, long, metavar, option, progDesc, short,
                            subparser)
import Options.Applicative.Help.Chunk (stringChunk)

import Polysemy (Member, Members, Sem)
import Polysemy.Resource (Resource)



---------------------------------------------------------------------------
-- CLI
---------------------------------------------------------------------------

-- | Run 'tibet' with cli command
runCommand :: Members [Resource, PrettyPrint, Console] r
  => Env -> Command -> Sem r ()
runCommand env = \case
    Shell selectedDicts -> do
      let newEnv = env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}
      translator newEnv
    Om -> putColorDoc magenta NewLine om
    ShowOption opt -> runShow env opt

runShow :: Member PrettyPrint r
  => Env -> Opt -> Sem r ()
runShow env opt = do
  let Labels labels = env.labels
  let filteredLabels = filterAvailable labels
  case opt of
    Names -> do
        let titles = sortById filteredLabels
        mapM_ (\label-> do
          putColorList
            [ (cyan, toText label.lfId <> ". ")
            , (green, label.label <> ". ")
            , (cyan, maybe "" (const "Year ") label.year)
            , (green, maybe "" (flip T.append ". " . toText) label.year)
            , (cyan, "From ")
            , (green, label.source <> " ")
            , (cyan, "to ")
            , (green, T.intercalate ", " (toList label.target) <> ".")]
          putColorDoc blue NewLine ""
          ) titles
        putColorDoc yellow NewLine $ T.pack $ "Available dictionaries: " <> show (length titles)
    Meta Nothing -> do
        mapM_ (\label -> do
            putColorList
              [ (cyan, toText label.lfId <> ". ")
              , (green, label.label)
              , (cyan, maybe "" (const ". Year ") label.year)
              , (green, maybe "" toText label.year)]
            putColorDoc blue NewLine ""
            putColorDoc blue NewLine label.about
            putColorList
              [ (cyan, "From ")
              , (green, label.source <> " ")
              , (cyan, "to ")
              , (green, T.intercalate ", " (toList label.target))]
            putColorDoc blue NewLine ""
            ) filteredLabels
        putColorDoc yellow NewLine $ T.pack $ "Available dictionaries: " <> show (length filteredLabels)
    Meta (Just n) -> do
        case find (\label -> n == label.lfId) filteredLabels of
            Nothing -> putColorDoc red NewLine "No such number of dictionary!"
            Just label -> do
              putColorDoc green NewLine $ toText label.lfId <> label.label
              putColorDoc blue NewLine label.about
  where
    toText :: Int -> Text
    toText n = T.pack $ show n
    sortById :: [LabelFull] -> [LabelFull]
    sortById = sortBy (\labelFull1 labelFull2 ->
        compare labelFull1.lfId labelFull2.lfId)
    filterAvailable :: [LabelFull] -> [LabelFull]
    filterAvailable = filter available


----------------------------------------------------------------------------
-- Command parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
parser :: ParserInfo Command
parser = modifyHeader
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
   <> command "show" (info (helper <*> showP) $ progDesc "Show names or meta of dictionaries")

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
