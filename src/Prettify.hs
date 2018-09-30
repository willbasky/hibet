-- | This module contains functions for colorful printing into terminal.

module Prettify
       ( Color (..)
       , putTextFlush
       , beautyPrint
       , boldCode
       , blueCode
       , bold
       , boldText
       , boldDefault
       , cyanCode
       , endLine
       , greenCode
       , italic
       , magentaCode
       , redCode
       , reset
       , resetCode
       , prompt
       , setColor
       , successMessage
       , warningMessage
       , errorMessage
       , infoMessage
       , skipMessage
       , yellowCode
       ) where

import           Data.Text (Text)
import           System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                                      ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
                                      SGR (..), setSGR, setSGRCode)
import           System.IO (hFlush, stdout)

import qualified Data.Text.IO as IO
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Ansi-terminal
----------------------------------------------------------------------------

-- Explicit flush ensures prompt messages are in the correct order on all systems.
putTextFlush :: Text -> IO ()
putTextFlush msg = do
    IO.hPutStrLn stdout msg
    hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

-- | Starts bold printing.
bold :: IO ()
bold = setSGR [SetConsoleIntensity BoldIntensity]

italic :: IO ()
italic = setSGR [SetItalicized True]

-- | Resets all previous settings.
reset :: IO ()
reset = do
    setSGR [Reset]
    hFlush stdout

-- | Takes list of formatting options, prints text using this format options.
beautyPrint :: [IO ()] -> Text -> IO ()
beautyPrint formats msg = do
    sequence_ formats
    IO.hPutStrLn stdout msg
    reset

prompt :: IO Text
prompt = do
    setColor Blue
    putTextFlush "  ->   "
    reset
    IO.getLine

boldText :: Text -> IO ()
boldText message = bold >> putTextFlush message >> reset

boldDefault :: Text -> IO ()
boldDefault message = boldText (" [" <> message <> "]")

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
    setColor color
    IO.hPutStrLn stdout $ "  " <> message
    reset

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green
infoMessage    = colorMessage Blue
skipMessage    = colorMessage Cyan

cyanCode, blueCode, boldCode, greenCode, magentaCode, redCode, resetCode, yellowCode :: Text
redCode = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
blueCode = T.pack $ setSGRCode [SetColor Foreground Vivid Blue]
cyanCode = T.pack $ setSGRCode [SetColor Foreground Vivid Cyan]
greenCode = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
yellowCode = T.pack $ setSGRCode [SetColor Foreground Vivid Yellow]
magentaCode = T.pack $ setSGRCode [SetColor Foreground Vivid Magenta]
boldCode = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
resetCode = T.pack $ setSGRCode [Reset]

endLine :: Text
endLine = "\n"