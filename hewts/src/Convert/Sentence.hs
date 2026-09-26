-- | Token-level sentence dispatcher.
--
-- Parses a token stream into a list of 'SpellItem' preserving every token:
-- syllables are recognized via the Convert.Grammar.Structure structures,
-- Tibetan digits ༠–༩ become 'Number', punctuation and whitespace tokens are
-- kept as 'Punct', and anything unrecognized is kept as 'Other' (nothing is
-- dropped).
module Convert.Sentence
    ( SpellItem (..)
    , pSentence
    ) where

import Convert.Grammar.Parser
    ( Parser
    , Spelling (..)
    , isPunctuationLike
    , pNumber
    , pPunctuation
    )
import Convert.Grammar.Stack (pStack)
import Convert.Grammar.Structure
    ( pStructure1
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
    , pStructure2
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
    , pStructure3
    , pStructure30
    , pStructure31
    , pStructure32
    , pStructure33
    , pStructure34
    , pStructure35
    , pStructure36
    , pStructure37
    , pStructure4
    , pStructure5
    , pStructure6
    , pStructure7
    , pStructure8
    , pStructure9
    )
import Convert.Grammar.Word (TibetanWord)
import Convert.Token (Token)
import qualified Text.Megaparsec as MP

data SpellItem
    = Syllable TibetanWord
    | Number [Token]
    | Punct [Token]
    | Other [Token]
    deriving (Show, Eq)

pSentence :: Spelling -> Parser [SpellItem]
pSentence spelling = MP.many (pItem spelling) <* MP.eof

pItem :: Spelling -> Parser SpellItem
pItem spelling =
    MP.choice
        [ Punct <$> MP.some pPunctuation
        , Number <$> MP.some pNumber
        , Syllable <$> MP.try (pStructure spelling)
        , Other <$> MP.some (MP.satisfy (not . isPunctuationLike))
        ]

-- A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). All structures are probed in
-- lookahead and the one with the longest match is then run for real, so
-- that input is actually consumed.
pStructure :: Spelling -> Parser TibetanWord
pStructure spelling = do
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
            [ pStructure28 spelling
            , pStructure29 spelling
            , pStructure30 spelling
            , pStructure31 spelling
            , pStructure32 spelling
            , pStructure33 spelling
            , pStructure34 spelling
            , pStructure35 spelling
            , pStructure36 spelling
            , pStructure37 spelling
            , pStructure21 spelling
            , pStructure22 spelling
            , pStructure23 spelling
            , pStructure24 spelling
            , pStructure13 spelling
            , pStructure14 spelling
            , pStructure15 spelling
            , pStructure16 spelling
            , pStructure17 spelling
            , pStructure18 spelling
            , pStructure19 spelling
            , pStructure20 spelling
            , pStructure9 spelling
            , pStructure10 spelling
            , pStructure11 spelling
            , pStructure12 spelling
            , pStructure27 spelling
            , pStructure26 spelling
            , pStructure25 spelling
            , pStructure4 spelling
            , pStructure5 spelling
            , pStructure6 spelling
            , pStructure7 spelling
            , pStructure8 spelling
            , pStructure1 spelling
            , pStructure2 spelling
            , pStructure3 spelling
            , pStack spelling
            ]
