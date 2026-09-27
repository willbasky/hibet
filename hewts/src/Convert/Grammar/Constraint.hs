-- | Public interface of the spelling grammar constraints - the single entry
-- point of the constraint layer.
--
-- Re-exports the individual constraint parsers ('pConstraint01' … 'pConstraint20')
-- and the generic-word parsers ('pConstraint21First'/'pConstraint21Rest'),
-- composed out of the token-level parser primitives in 'Convert.Grammar.Parser'.
-- The constraint parsers are building blocks (prefix / root / vowel / suffix /
-- postfix / basic-syllable fragments) composed by 'Convert.Grammar.Structure'
-- into complete syllable spelling structures; the generic-word parsers spell
-- whatever the book's structures leave unspelled.
module Convert.Grammar.Constraint
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
    , pConstraint21First
    , pConstraint21Rest
    ) where

import Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01Sanskrit
    , pConstraint01WithLong
    )
import Convert.Grammar.Constraint.Constraint08 (pConstraint08)
import Convert.Grammar.Constraint.Constraint09 (pConstraint09)
import Convert.Grammar.Constraint.Constraint10 (pConstraint10)
import Convert.Grammar.Constraint.Constraint11 (pConstraint11)
import Convert.Grammar.Constraint.Constraint12 (pConstraint12)
import Convert.Grammar.Constraint.Constraint13 (pConstraint13)
import Convert.Grammar.Constraint.Constraint14 (pConstraint14)
import Convert.Grammar.Constraint.Constraint15 (pConstraint15)
import Convert.Grammar.Constraint.Constraint16
    ( pConstraint16Da
    , pConstraint16Sa
    )
import Convert.Grammar.Constraint.Constraint17
    ( pConstraint17Ra
    , pConstraint17Ya
    )
import Convert.Grammar.Constraint.Constraint18 (pConstraint18)
import Convert.Grammar.Constraint.Constraint19 (pConstraint19)
import Convert.Grammar.Constraint.Constraint20 (pConstraint20)
import Convert.Grammar.Constraint.Constraint21
    ( pConstraint21First
    , pConstraint21Rest
    )
