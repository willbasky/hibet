{- Tibetan spelling grammar 4.8 and 5.1: the superfix data

The roots each superfix letter may gate (4.8) and the (root, subfix) rows it
takes below them (5.1), stated once: the book's structure 2 parses by them
('Convert.Grammar.Constraint.Constraint08'), the generic word's superfix window
checks against them ('Convert.Grammar.Constraint.Constraint21'), and the Wylie
notation writes solid only what they allow ('Convert.Wylie.Tables'). None of
the three owns the table.
-}

module Convert.Grammar.Superfix
    ( rSuperfixRoots
    , lSuperfixRoots
    , sSuperfixRoots
    , superfixRoots
    , superfixCombos
    ) where

import Convert.Token (Consonant (..))

-- | The roots the superfix ར may gate (rule 4.8).
rSuperfixRoots :: [Consonant]
rSuperfixRoots = [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cb, Cm, Cts, Cdz]

-- | The roots the superfix ལ may gate (rule 4.8).
lSuperfixRoots :: [Consonant]
lSuperfixRoots = [Ck, Cg, Cng, Cc, Cj, Ct, Cd, Cp, Cb, Ch]

-- | The roots the superfix ས may gate (rule 4.8).
sSuperfixRoots :: [Consonant]
sSuperfixRoots = [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cp, Cb, Cm, Cts]

-- | The roots each superfix letter may gate: only ར ལ ས take anything below
-- them, any other letter is no superfix.
superfixRoots :: Consonant -> [Consonant]
superfixRoots Cr = rSuperfixRoots
superfixRoots Cl = lSuperfixRoots
superfixRoots Cs = sSuperfixRoots
superfixRoots _ = []

-- | The (root, subfix) rows a superfix letter takes below those roots
-- (rules 4.8, 5.1): one letter only, so a run of two never matches a row;
-- @l@ takes none.
superfixCombos :: Consonant -> [(Consonant, Consonant)]
superfixCombos Cr =
    [ (Ck, Cy)
    , (Cg, Cy)
    , (Cm, Cy)
    , (Cb, Cw)
    , (Cts, Cw)
    , (Cg, Cw)
    ]
superfixCombos Cl = []
superfixCombos Cs =
    [ (Ck, Cy)
    , (Cg, Cy)
    , (Cp, Cy)
    , (Cb, Cy)
    , (Cm, Cy)
    , (Ck, Cr)
    , (Cg, Cr)
    , (Cp, Cr)
    , (Cb, Cr)
    , (Cm, Cr)
    , (Cn, Cr)
    ]
superfixCombos _ = []
