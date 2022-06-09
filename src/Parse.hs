

module Parse
       (
       -- * Convertors
         toTibetan
       , toWylie
       -- * Make HashMap (W -> T) and (T -> W) from syllables
      --  , mkSyllablesMap
       -- * Parsers
       , tibetanWord
       , wylieWord
       , parseWylieInput
       , parseTibetanInput
       , parseExcept
       , parseEither
       -- * Radix trees
       , mkWylieRadex
       , mkTibetanRadex
       -- * splitter
       , splitSyllables
       -- * Data types
       , WylieTibetMap
       , TibetWylieMap
       , TibetScript(..)
       , fromTibetScript
       , WylieScript(..)
       , fromWylieScript
       , TibetSyllable(..)
       , WylieSyllable(..)
       , lookupWylieScript
       , lookupTibetScript
       ) where

import Parse.SyllableLines (splitSyllables)
import Parse.TibetanWord (tibetanWord)
import Parse.Type (TibetScript (..), TibetSyllable (..), TibetWylieMap, WylieScript (..),
                   WylieSyllable (..), WylieTibetMap, fromTibetScript, fromWylieScript, parseEither,
                   parseExcept)
import Parse.WylieWord (wylieWord)
import Type (HibetError (..))

import Control.Monad.Except (Except, liftEither)
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
    -> Except HibetError [WylieScript]
    -> Except HibetError [TibetScript]
toTibetan bi ts = do
    txt <- ts
    let look :: WylieSyllable -> Maybe TibetSyllable
        look = flip HM.lookup bi
    liftEither $ flip traverse txt $ \case
      ScriptWylie w -> case look w of
        Nothing  -> Left NotFound
        Just res -> Right $ ScriptTibet res
      NonScriptWylie t -> Right $ NonScriptTibet t

-- | Convert parsed tibetan text to wylie.
toWylie
    :: TibetWylieMap
    -> Except HibetError [TibetScript]
    -> Except HibetError [WylieScript]
toWylie hm ets = do
    ts <- ets
    let look :: TibetSyllable -> Maybe WylieSyllable
        look = flip HM.lookup hm
    liftEither $ flip traverse ts $ \case
      ScriptTibet w -> case look w of
        Nothing  -> Left NotFound
        Just res -> Right $ ScriptWylie res
      NonScriptTibet t -> Right $ NonScriptWylie t

-- Radix stuff
-- Radix structure used just to check query's validity.

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
parseWylieInput :: RadixTree () -> Text -> Except HibetError [WylieScript]
parseWylieInput radix txt  = do
    ls <- parseExcept wylieWord txt
    pure $ map (lookupWylieScript radix) ls

-- | Parse text to tibetan or fail.
parseTibetanInput :: RadixTree () -> Text -> Except HibetError [TibetScript]
parseTibetanInput radix txt  = do
    ts <- parseExcept tibetanWord txt
    pure $ map (lookupTibetScript radix) ts

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
