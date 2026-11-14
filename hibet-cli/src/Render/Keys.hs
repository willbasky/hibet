module Render.Keys
    ( Key (..)
    , withRawKeys
    )
where

import Control.Exception (bracket)
import Control.Monad (join)
import System.IO
    ( BufferMode (NoBuffering)
    , hGetBuffering
    , hGetChar
    , hIsEOF
    , hSetBuffering
    , stdin
    )
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalAttributes
    , TerminalMode (..)
    , TerminalState (..)
    , getTerminalAttributes
    , setTerminalAttributes
    , withoutMode
    )
import System.Timeout (timeout)

-- | A key press the pager acts on.
data Key
    = LineUp
    | LineDown
    | PageUp
    | PageDown
    | Quit
    | Other
    deriving stock (Eq, Show)

-- | Run @action@ with a key reader while the terminal is in raw mode. A pager
-- cannot use 'Haskeline.getInputChar': it drops everything non-printable, so
-- arrows, PgUp/PgDn and Ctrl chords never reach it. Raw mode also keeps the
-- terminal from echoing or waiting for Enter, and turns Ctrl-C into a byte the
-- pager reads as 'Quit'.
withRawKeys :: (IO Key -> IO a) -> IO a
withRawKeys action = bracket enter leave (const (action readKey))
    where
        enter = do
            attributes <- getTerminalAttributes stdInput
            setTerminalAttributes stdInput (raw attributes) Immediately
            buffering <- hGetBuffering stdin
            hSetBuffering stdin NoBuffering
            pure (attributes, buffering)

        leave (attributes, buffering) = do
            hSetBuffering stdin buffering
            setTerminalAttributes stdInput attributes Immediately

raw :: TerminalAttributes -> TerminalAttributes
raw attributes =
    foldl
        withoutMode
        attributes
        [ EnableEcho
        , ProcessInput
        , ExtendedFunctions
        , KeyboardInterrupts
        ]

readKey :: IO Key
readKey = do
    first <- byte Nothing
    case first of
        Nothing -> pure Quit
        Just '\ESC' -> escape
        Just c -> pure (plain c)
    where
        plain :: Char -> Key
        plain = \case
            'q' -> Quit
            'j' -> LineDown
            'k' -> LineUp
            ' ' -> PageDown
            'b' -> PageUp
            '\x03' -> Quit -- Ctrl-C
            '\x02' -> PageUp -- Ctrl-B, Vim
            '\x06' -> PageDown -- Ctrl-F, Vim
            '\x0E' -> LineDown -- Ctrl-N, Emacs
            '\x10' -> LineUp -- Ctrl-P, Emacs
            '\x16' -> PageDown -- Ctrl-V, Emacs
            _ -> Other

        -- ESC alone is skipped; a sequence continues within a blink.
        escape :: IO Key
        escape = do
            next <- byte (Just escapeDelay)
            case next of
                Just 'v' -> pure PageUp -- M-v, Emacs
                Just '[' -> sequenceKey
                Just 'O' -> sequenceKey -- application cursor keys
                _ -> pure Other

        sequenceKey :: IO Key
        sequenceKey = do
            next <- byte (Just escapeDelay)
            case next of
                Just 'A' -> pure LineUp
                Just 'B' -> pure LineDown
                Just '5' -> tilde PageUp
                Just '6' -> tilde PageDown
                _ -> pure Other

        tilde :: Key -> IO Key
        tilde key = do
            next <- byte (Just escapeDelay)
            pure (if next == Just '~' then key else Other)

        -- Nothing means end of input; a timed-out byte means the previous ESC
        -- was a lone key press.
        byte :: Maybe Int -> IO (Maybe Char)
        byte wait = case wait of
            Nothing -> blocking
            Just ms -> join <$> timeout ms blocking
            where
                blocking = do
                    eof <- hIsEOF stdin
                    if eof then pure Nothing else Just <$> hGetChar stdin

-- | Long enough for the rest of an escape sequence, short enough to not feel
-- stuck after a lone ESC.
escapeDelay :: Int
escapeDelay = 100000
