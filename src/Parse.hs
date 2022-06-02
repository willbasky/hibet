

module Parse
       (
       -- * Convertors from parsed text
         toTibetan
       , toWylie
       -- * Make bimap from syllables
       , makeBi
       -- * Parsers
       , parseWylieInput
       , parseTibetanInput
       -- * Radix trees
       , makeWylieRadexTree
       , makeTibetanRadexTree
       -- * splitter
       , splitSyllables
       -- * Data types
       , BimapWylieTibet
       , TibetScript(..)
       , fromTibetScript
       , WylieScript(..)
       , fromWylieScript
       , TibetSyllable(..)
       , WylieSyllable(..)
       , lookupWylieScript
       , lookupTibetScript
       ) where

import Parse.Script (makeBi, splitSyllables, walLines, tibetanScript)
import Parse.Type
import Type (HibetError (..))
import Utility (toText)

import Control.Exception (SomeException)
import Control.Monad.Except (Except, liftEither)
import qualified Data.Bimap as Bi
import Data.RadixTree (RadixTree, fromFoldable_, lookup)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Data.Text.Lines as Line



-- | Convert parsed wylie text to tibetan script.
-- TODO: Check perfomance on big input
toTibetan
    :: BimapWylieTibet
    -> Except HibetError [WylieScript]
    -> Except HibetError [TibetScript]
toTibetan bi ts = do
    txt <- ts
    let look :: WylieSyllable -> Either SomeException TibetSyllable
        look = flip Bi.lookup bi
    liftEither $ traverse (\case
      ScriptWylie w -> case look w of
        Left err  -> Left $ BimapError $ toText err
        Right res -> Right $ ScriptTibet res
      NonScriptWylie t -> Right $ NonScriptTibet t
      ) txt

-- | Convert parsed tibetan text to wylie.
toWylie
    :: BimapWylieTibet
    -> Except HibetError [TibetScript]
    -> Except HibetError [WylieScript]
toWylie bi ts = do
    txt <- ts
    let look :: TibetSyllable -> Either SomeException WylieSyllable
        look = flip Bi.lookupR bi
    liftEither $ traverse (\case
      ScriptTibet w -> case look w of
        Left err  -> Left $ BimapError $ toText err
        Right res -> Right $ ScriptWylie res
      NonScriptTibet t -> Right $ NonScriptWylie t
      ) txt


lookupWylieScript :: RadixTree a -> Text -> WylieScript
lookupWylieScript radix t = case lookup radix t of
  Nothing     -> NonScriptWylie t
  Just (t',_) -> ScriptWylie $ WylieSyllable t'

lookupTibetScript :: RadixTree a -> Text -> TibetScript
lookupTibetScript radix t = case lookup radix t of
  Nothing     -> NonScriptTibet t
  Just (t',_) -> ScriptTibet $ TibetSyllable t'

{- | Parse text to wylie or fail.
'search' gives parsing of dirty strings, e.g.
@
> parseWylieInput radix "(balka)"
> parseWylieInput radix "(balkana)"
ExceptT (Identity (Right [([],[["balka"]])]))
@
it reads radixed words and drops non-radixed in anyway.
Therefore dirty wylie text should parsed better beforehand.
There are two approaches
1. Add non-wylie chars and strings to sillabies
and then parse (1.balka) -> ["(", "1", "." "balka", ")"]
and then it become "(1.བལྐ)" or "(༡.བལྐ)", etc...
2. Parse dirty text by separating non-wylie stuff from wylie
and then 'map lookupLemient', 'lookupLemient' left non-wylie untouched
and convert wylie to tibetan script.
@
> map (lookupLenient radix) ["(", "balka", ")"]
["(","balka",")"]
@
In both 1 and 2 a dirty text must be separated roughly,
hence it is better to keep syllabies non-wylie-free
therefore second approach is better.

At first, current transforming will be reduced (broken) to flat list.
Something won't be consumed, and droped.
At second, current parsers will be refactored to more and more correct result against flat list.

-}
parseWylieInput :: RadixTree () -> Text -> Except HibetError [WylieScript]
parseWylieInput radix txt  = do
    ls <- parseT walLines "" txt
    pure $ map (lookupWylieScript radix) ls

-- | Parse text to tibetan or fail.
parseTibetanInput :: RadixTree () -> Text -> Except HibetError [TibetScript]
parseTibetanInput radix txt  = do
    ts <- parseT tibetanScript "" txt
    pure $ map (lookupTibetScript radix) ts

-- | Make wylie radix tree from syllables.
makeWylieRadexTree :: Text -> RadixTree ()
makeWylieRadexTree syls =
    let linedSyls = Line.lines $ Line.fromText syls
        headPart = map (T.takeWhile (/= '|')) linedSyls
    in  fromFoldable_ headPart

-- | Make tibetan radix tree from syllables.
makeTibetanRadexTree :: Text -> RadixTree ()
makeTibetanRadexTree syls =
    let linedSyls = Line.lines $ Line.fromText syls
        lastPart = map (T.takeWhileEnd (/= '|')) linedSyls
    in  fromFoldable_ lastPart
