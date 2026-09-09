{- | Public interface for the Tibetan spelling grammar layer.

This is the single public door into the spelling grammar: the shared token
parser primitive 'Parser' / 'parseEither', the syllable-boundary helpers
('pPunctuation', 'pNumber', 'isPunctuationLike'), the 37 syllable spelling
structures ('pStructure1' … 'pStructure37'), and the underlying constraint
parsers ('pConstraint01' … 'pConstraint20').

The structures recognize a single syllable out of a 'Token' stream. They are
composed downward from the Rule/Constraint modules and consumed upward by
'Convert.Sentence'. The implementation modules living under 'Convert.Grammar.*'
(Parser, Rule, Rule.Constraint*) are internal.
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
    , pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    , pConstraint08
    , pConstraint09
    , pConstraint10
    , pConstraint11
    , pConstraint12
    , pConstraint13
    , pConstraint14
    , pConstraint15
    , pConstraint16Da
    , pConstraint16Sa
    , pConstraint17Ra
    , pConstraint17Ya
    , pConstraint18
    , pConstraint19
    , pConstraint20
    ) where

import Convert.Grammar.Parser (Parser, parseEither, pPunctuation, pNumber, isPunctuationLike)
import Convert.Grammar.Constraint
  ( pConstraint01
  , pConstraint01WithLong
  , pConstraint01Sanskrit
  , pConstraint08
  , pConstraint09
  , pConstraint10
  , pConstraint11
  , pConstraint12
  , pConstraint13
  , pConstraint14
  , pConstraint15
  , pConstraint16Da
  , pConstraint16Sa
  , pConstraint17Ra
  , pConstraint17Ya
  , pConstraint18
  , pConstraint19
  , pConstraint20
  )
import Convert.Grammar.Structure
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
