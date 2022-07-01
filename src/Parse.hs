

module Parse
       (
       -- * Convertors
         toTibetan
       , toWylie
       -- * Parsers
       , tibetanWord
       , wylieWord
       , parseWylieInput
       , parseTibetanInput
       , parseEither
       -- * Radix trees
       , mkWylieRadex
       , mkTibetanRadex
       -- * splitter
       , splitSyllables
       -- * Data types
       , WylieTibetMap
       , TibetWylieMap
       , Script (Script)
       , ScriptType(..)
       , fromScripts
       ) where

import Parse.SyllableLines (splitSyllables)
import Parse.TibetanWord (tibetanWord)
import Parse.Type (Script (..), ScriptType (..), TibetWylieMap, WylieTibetMap, fromScripts,
                   parseEither)
import Parse.WylieWord (wylieWord)
import Type (HibetError (..))

import qualified Data.HashMap.Strict as HM
import Data.RadixTree (RadixTree, fromFoldable_, lookup)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lines as Line
import Prelude hiding (lookup)

-- import qualified Debug.Trace as Debug



-- | Convert parsed wylie text to tibetan script.
-- TODO: Check perfomance on big input
toTibetan
    :: WylieTibetMap
    -> [Script 'Wylie]
    -> Either HibetError [Script 'Tibet]
toTibetan wtMap wList = do
    let look :: Script 'Wylie -> Maybe (Script 'Tibet)
        look = flip HM.lookup wtMap
    flip traverse wList $ \w ->
      case look w of
        Nothing  -> Left NotFound
        Just res -> Right res

-- | Convert parsed tibetan text to wylie.
toWylie
    :: TibetWylieMap
    -> [Script 'Tibet]
    -> Either HibetError [Script 'Wylie]
toWylie twMap tList = do
    let look :: Script 'Tibet -> Maybe (Script 'Wylie)
        look = flip HM.lookup twMap
    flip traverse tList $ \t ->
      case look t of
        Nothing  -> Left NotFound
        Just res -> Right res

-- Radix stuff
-- Radix structure used just to check query's validity.

lookupWylieScript :: RadixTree a -> Text -> Either HibetError (Script 'Wylie)
lookupWylieScript radix txt = case lookup radix txt of
  Nothing    -> Left $ NotSyllable txt
  Just (t,_) -> Right $ Script t

lookupTibetScript :: RadixTree a -> Text -> Either HibetError (Script 'Tibet)
lookupTibetScript radix txt = case lookup radix txt of
  Nothing    -> Left $ NotSyllable txt
  Just (t,_) -> Right $ Script t

{- | Parse text to wylie or fail.
'search' gives parsing of dirty strings, e.g.
@
> parseWylieInput radix "(balka)"
> parseWylieInput radix "(balkana)"
Right [([],[["balka"]])])
@
it reads radixed words and drops non-radixed in anyway.
Therefore dirty wylie text should parsed better beforehand.
There are two approaches
1. Add non-wylie chars and strings to sillables
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
hence it is better to keep syllables non-wylie-free
therefore second approach is better.

At first, current transforming will be reduced (broken) to flat list.
Something won't be consumed, and dropped.
At second, current parsers will be refactored to more and more correct result against flat list.

-}
parseWylieInput :: RadixTree ()
  -> Text
  -> Either HibetError [Script 'Wylie]
parseWylieInput radix txt  = do
    ls <- parseEither wylieWord txt
    traverse (lookupWylieScript radix) ls

-- | Parse text to tibetan or fail.
parseTibetanInput :: RadixTree ()
  -> Text
  -> Either HibetError [Script 'Tibet]
parseTibetanInput radix txt  = do
    ts <- parseEither tibetanWord txt
    traverse (lookupTibetScript radix) ts

-- | Make wylie radix tree from syllables.
mkWylieRadex :: Text -> RadixTree ()
mkWylieRadex syls =
    let linedSyls = Line.lines $ Line.fromText syls
        headPart = map (T.takeWhile (/= '|')) linedSyls
    in  fromFoldable_ headPart

-- | Make tibetan radix tree from syllables.
mkTibetanRadex :: Text -> RadixTree ()
mkTibetanRadex syls =
    let linedSyls = Line.lines $ Line.fromText syls
        lastPart = map (T.takeWhileEnd (/= '|')) linedSyls
    in  fromFoldable_ lastPart
