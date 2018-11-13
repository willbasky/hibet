{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli
       ( trans
       ) where

import           Data.Text (Text)
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitDate, gitDirty, gitHash)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help,
                                      helper, info, infoHeader, infoOption, long, progDesc, short,
                                      subparser)
import           Options.Applicative.Help.Chunk (stringChunk)

import           Paths_tibet (version)
import           Prettify (blueCode, boldCode, endLine, magentaCode, putTextFlush, redCode,
                           resetCode, yellowCode)
import           Tibet (start)
import           Titles (showTitles)

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Command data types
----------------------------------------------------------------------------

-- | Represent all available commands
data Command
    -- | @shell@ command launch translating shell
    = Shell
    | Om
    | ShowTitles

---------------------------------------------------------------------------
-- CLI
---------------------------------------------------------------------------

trans :: IO ()
trans = execParser prsr >>= runCommand

-- | Run 'tibet' with cli command
runCommand :: Command -> IO ()
runCommand = \case
    Shell -> start
    Om -> putTextFlush $ magentaCode <> om <> resetCode
    ShowTitles -> showTitles

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
prsr :: ParserInfo Command
prsr = modifyHeader
    $ info ( helper <*> versionP <*> shellP)
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
    sVersion = blueCode <> boldCode <> "TibetCli " <> "v" <> T.pack (showVersion version) <> resetCode
    sHash = " ➤ " <> blueCode <> boldCode <> "Git revision: " <> resetCode <> $(gitHash)
    sDate = " ➤ " <> blueCode <> boldCode <> "Commit date:  " <> resetCode <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode

-- All possible commands.
shellP :: Parser Command
shellP = subparser
    $ command "shell" (info (helper <*> pure Shell) $ progDesc "Start translate shell")
   <> command "om" (info (helper <*> pure Om) $ progDesc "Print Om to a terminal")
   <> command "show" (info (helper <*> pure ShowTitles) $ progDesc "Show titles of dictionaris")

----------------------------------------------------------------------------
-- Beauty util
----------------------------------------------------------------------------

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p {infoHeader = stringChunk $ T.unpack artHeader}

artHeader :: Text
artHeader = yellowCode <> [text|
$endLine
                                  .+-
                           .:+sydNMd`
      `ymMMmy:         :sdMMMMMMMd/
      /MMMMMMMm/    `oNMMMMNdyo:`
       `.-:/sdMMy` +NNho/-`
              `oNdhy:
                `y/
          /yyyyyyyyyyyyyyy+   -syyyyyyyyyyyys`   oyyy.
         yMMMMMMMMMMMMMMMMN  -MMMMMMMMMMMMMMN`  `dMMd`
         :yMMMhsssssssNMMMy  `omMMmysssssss+`    `oo`
         `dMs`        :MMM.   /Md:
         sM:           hMm   `Ny
         ms            :My  `sMdddddhyo:`
        oNNMMMMNds:     Ns  yMMMMMMMMMMMMy-
       -MMMMMMMMMMMNo`  do  .ooo+///+oymMMMy`
        ://:-.-:+ymMMm: ho              -hMMd`
                   .+dMoho                +MMo
                      .sNo                 oMN
                        `.                  NM`
                                            sM.
                                            /M.
                                            -M.
                                            .M.
                                            `M.
                                            `N.
                                             d`
            |] <> resetCode

om :: Text
om = [text|
$endLine
                             /hd+
                           /Nd::ohs.
                          /MM- .NMm+
                          :MMdosMMN.        `.-.
                  `oNNmdyo:/dMMMms/ohmNNNNNmy:`
                 `hMMMMMNmNN+.` .dmhyyso+/.
                 `.` `...:+ohy:.+.
                              .-     `.
              :yddddo `/mmNNmmdddmNNNm-  .mhhd-
             -mMMMMN. yMMMMMMMMMMMMMm+  +NMMMMo
             dMMMMM/  .+smMMMMMMMMd:.  :NMMMMMs
             `mMMMh    .hMMMs:hMMMM/    .mMMMMo
              +MMM/   -NNh/`   :mMMN:    .mMMMo
              `NMm    ms``-.`   `oNMN-    :MMM+
               mMy   -o+dNMMMNh:  .hMN-    hMM/
               yMy   .shydMMMMMM+   /Nm.   :MM:
               :Mh    `   :NMMMMm    -mm-  `mM`
                dN.        yMMMMo      sN-  sm
                `ym.      :NMMMo`       /d: /d
                  -yo:`.:yMMdo.          `yo.s
                     .--:--`               +y+
                                            :+
            |]
