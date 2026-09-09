{- | Public interface for the Tibetan spelling grammar layer.

Exposes the shared token parser primitive 'Parser' / 'parseEither', the
syllable-boundary helpers ('pPunctuation', 'pNumber', 'isPunctuationLike'),
and the 37 syllable spelling structures ('pStructure1' … 'pStructure37') that
recognize a single syllable out of a 'Token' stream. The structures are
composed downward from the Rule/Constraint modules and consumed upward by
'Convert.Sentence'.
-}

module Convert.Grammar
    ( Parser
    , parseEither
    , pPunctuation
    , pNumber
    , isPunctuationLike
    , pStructure1
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
    ) where

import Convert.Grammar.Parser (Parser, parseEither, pPunctuation, pNumber, isPunctuationLike)
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
