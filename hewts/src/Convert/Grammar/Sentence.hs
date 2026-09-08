{-
Token-level sentence dispatcher.

Parses a token stream into a list of SpellItem preserving every token:
syllables are recognized via the Convert.Grammar.Rule structures,
punctuation and whitespace tokens are kept as Punct, and anything
unrecognized is kept as Other (nothing is dropped).
-}

module Convert.Grammar.Sentence
    ( SpellItem (..)
    , pSentence
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Rule
  ( pStructure1
  , pStructure2
  , pStructure3
  , pStructure4
  , pStructure5
  , pStructure6
  , pStructure7
  , pStructure8
  , pStructure9
  , pStructure10
  , pStructure11
  , pStructure12
  , pStructure13
  , pStructure14
  , pStructure15
  , pStructure16
  , pStructure17
  , pStructure18
  , pStructure19
  , pStructure20
  , pStructure21
  , pStructure22
  , pStructure23
  , pStructure24
  , pStructure25
  , pStructure26
  , pStructure27
  , pStructure28
  , pStructure29
  , pStructure30
  , pStructure31
  , pStructure32
  , pStructure33
  , pStructure34
  , pStructure35
  , pStructure36
  , pStructure37
  )
import Convert.Token (Token)
import qualified Text.Megaparsec as MP

data SpellItem
    = Syllable [Token]
    | Punct [Token]
    | Other [Token]
    deriving (Show, Eq)

pSentence :: Parser [SpellItem]
pSentence = MP.many pItem <* MP.eof

pItem :: Parser SpellItem
pItem =
    MP.choice
        [ Punct <$> MP.some GP.pPunctuation
        , Syllable <$> MP.try pStructure
        , Other <$> MP.some (MP.satisfy (not . GP.isPunctuationLike))
        ]

-- A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). All structures are probed in
-- lookahead and the one with the longest match is then run for real, so
-- that input is actually consumed.
pStructure :: Parser [Token]
pStructure = do
    start <- MP.getInput
    let probe p = do
            r <-
                MP.option
                    Nothing
                    (Just <$> MP.try (MP.lookAhead ((,) <$> p <*> MP.getInput)))
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (length start - length end, p)
    best <- foldl' better Nothing <$> mapM probe parses
    maybe MP.empty snd best
  where
    better Nothing c = c
    better c Nothing = c
    better acc@(Just (m, _)) c@(Just (n, _))
        | m >= n = acc
        | otherwise = c
    parses =
        [ pStructure28, pStructure29, pStructure30, pStructure31, pStructure32
        , pStructure33, pStructure34, pStructure35, pStructure36, pStructure37
        , pStructure21, pStructure22, pStructure23, pStructure24
        , pStructure13, pStructure14, pStructure15, pStructure16
        , pStructure17, pStructure18, pStructure19, pStructure20
        , pStructure9, pStructure10, pStructure11, pStructure12
        , pStructure27, pStructure26, pStructure25
        , pStructure4, pStructure5, pStructure6, pStructure7, pStructure8
        , pStructure1, pStructure2, pStructure3
        ]