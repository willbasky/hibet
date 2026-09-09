module Main (main) where

import Convert (SpellItem (..), splitSentences, syllables)
import Convert.Token (tokenRaw)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

main :: IO ()
main = do
    input <- T.IO.getContents
    case splitSentences input of
        Left e -> T.IO.putStrLn ("parse error: " <> e)
        Right items -> T.IO.putStrLn (T.unlines (map itemLine items))
    case syllables input of
        Left _ -> pure ()
        Right syles -> T.IO.putStrLn (T.unlines syles)
  where
    itemLine :: SpellItem -> Text
    itemLine item = tag <> " " <> T.concat (map tokenRaw tokens)
      where
        (tag, tokens) = case item of
            Syllable ts -> ("S", ts)
            Number ts -> ("N", ts)
            Punct ts -> ("P", ts)
            Other ts -> ("O", ts)