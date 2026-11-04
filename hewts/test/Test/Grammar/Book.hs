-- Ожидания: книга (Nyima Tashi) — таблица 37 структур, наборы букв 4.2–4.14
-- и генератор по ролям.

-- | Wave 10: the book's 37 syllable structures as data, the letter sets of
-- rules 4.2-4.14 as a second instance of the data, and the oracle property
-- "a word is read iff the roles are a legal book composition".
--
-- The table below is independent of the parsers: the witnesses and the sets are
-- copied from the book, not imported from 'Convert.Grammar.Superfix' or the
-- parser's tables. That is what makes the check non-tautological.
--
-- The model (recorded in 'docs/ROADMAP.md', Wave 10): the book's prose says a
-- bare consonant is naturally accompanied by the vowel a (1.2), while the
-- formal G_i do not encode it (4.7 has Vowel = {i,u,e,o}); the oracle reads the
-- book as 4.1-4.21 plus that implicit a. So a bare root carries a, and the 30
-- "prefix + root" forms without a written vowel are legal, not divergences.
module Test.Grammar.Book (tests, bookSyllable) where

import Convert.Grammar.Parser
    ( SpellParser
    , liftP
    , parseEither
    , runSpell
    , takeStacks
    )
import Convert.Grammar.Structure
import Convert.Grammar.Syllable
    ( Role (..)
    , Stack (..)
    , Subfixes (..)
    , TibetanSyllable
    , VowelSlot (..)
    , finalsTokens
    )
import Convert.Token
    ( Consonant (..)
    , FinalMark
    , Token
    , TokenCanonical (..)
    , Vowel (..)
    , fromSubjoined
    , toSubjoined
    , tokenCanonical
    , tokenRaw
    )
import Convert.Token.Unicode (unicodeOf)
import Convert.Token.Wylie (wylieConsonant, wylieVowel)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Falsify (Property, assert, gen)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import qualified Text.Megaparsec as MP

-- | One book structure: its number, the rules it composes, a witness and the
-- roles the witness's stacks must carry.
data BookStructure = BookStructure
    { bookNumber :: Int
    , bookRules :: [Text]
    , bookWitness :: Text
    , bookRoles :: [Role]
    }
    deriving (Show)

-- | The table of 37 structures. Rule numbers are the book's 4.1-4.20.
--
-- The witnesses are the shortest forms the rule allows; each is written in
-- Unicode, so the same table checks the glyph side. For 4.20 the a-chung is a
-- root when a vowel or a second root follows, and the word's last suffix when
-- it stands bare - the documented reading of 'Convert.Grammar.Constraint20'.
bookStructures :: [BookStructure]
bookStructures =
    [ BookStructure 1 ["4.1"] "སོ" [RoleRoot]
    , BookStructure 2 ["4.8"] "སྒ" [RoleRoot]
    , BookStructure 3 ["4.9"] "སྲོ" [RoleRoot]
    , BookStructure 4 ["4.10"] "རྐྱུ" [RoleRoot]
    , BookStructure 5 ["4.11"] "བལྟ" [RolePrefix, RoleRoot]
    , BookStructure 6 ["4.12"] "འདྲ" [RolePrefix, RoleRoot]
    , BookStructure 7 ["4.13"] "བརྒྱ" [RolePrefix, RoleRoot]
    , BookStructure 8 ["4.14"] "བཏ" [RolePrefix, RoleRoot]
    , BookStructure 9 ["4.14", "4.15"] "བཏག" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 10 ["4.11", "4.15"] "བརྒན" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 11 ["4.12", "4.15"] "མགྱས" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 12 ["4.13", "4.15"] "བརྒྱས" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure
        13
        ["4.14", "4.15", "4.16"]
        "བཏནད"
        [RolePrefix, RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        14
        ["4.11", "4.15", "4.16"]
        "བརྟགས"
        [RolePrefix, RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        15
        ["4.12", "4.15", "4.16"]
        "དགྱིགས"
        [RolePrefix, RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        16
        ["4.13", "4.15", "4.16"]
        "བསྒྱུགས"
        [RolePrefix, RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure 17 ["4.15"] "བིག" [RoleRoot, RoleSuffix]
    , BookStructure 18 ["4.8", "4.15"] "རྒིད" [RoleRoot, RoleSuffix]
    , BookStructure 19 ["4.9", "4.15"] "ཁྱོའ" [RoleRoot, RoleSuffix]
    , BookStructure 20 ["4.10", "4.15"] "སྐྲོན" [RoleRoot, RoleSuffix]
    , BookStructure
        21
        ["4.1", "4.15", "4.16"]
        "བགས"
        [RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        22
        ["4.8", "4.15", "4.16"]
        "རྨགས"
        [RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        23
        ["4.9", "4.15", "4.16"]
        "རློམས"
        [RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure
        24
        ["4.10", "4.15", "4.16"]
        "རྩྭོངས"
        [RoleRoot, RoleSuffix, RolePostfix]
    , BookStructure 25 ["4.17"] "དྲྭ" [RoleRoot]
    , BookStructure 26 ["4.18"] "ཧྥ" [RoleRoot]
    , BookStructure 27 ["4.19"] "ཧྥེལ" [RoleRoot, RoleSuffix]
    , BookStructure 28 ["4.1", "4.20"] "ཧིའེ" [RoleRoot, RoleRoot]
    , BookStructure 29 ["4.8", "4.20"] "སྨོའྨ" [RoleRoot, RoleRoot]
    , BookStructure 30 ["4.9", "4.20"] "སླིའྔ" [RoleRoot, RoleRoot]
    , BookStructure 31 ["4.10", "4.20"] "སྒྱའོ" [RoleRoot, RoleRoot]
    , BookStructure 32 ["4.11", "4.20"] "བལྟའ" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 33 ["4.12", "4.20"] "མཁྲའ" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 34 ["4.13", "4.20"] "བསྐྲའ" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 35 ["4.14", "4.20"] "བཟིའ" [RolePrefix, RoleRoot, RoleSuffix]
    , BookStructure 36 ["4.17", "4.20"] "གྲྭའ" [RoleRoot, RoleSuffix]
    , BookStructure 37 ["4.18", "4.20"] "ཧྥོའ" [RoleRoot, RoleSuffix]
    ]

ruleNames :: [Text]
ruleNames = ["4." <> T.pack (show n) | n <- [1 .. 20]]

tests :: TestTree
tests =
    testGroup
        "book structures"
        [ testCase "the table covers structures 1..37 once" $
            sort (map bookNumber bookStructures) @?= [1 .. 37]
        , testCase "every structure names the book's rules" $
            [ (bookNumber s, r)
            | s <- bookStructures
            , r <- bookRules s
            , r `notElem` ruleNames
            ]
                <> [(bookNumber s, "none") | s <- bookStructures, null (bookRules s)]
                @?= []
        , testGroup
            "every structure accepts its book witness"
            [ testCase
                (show (bookNumber s) <> ": " <> T.unpack (bookWitness s))
                (checkWitness s)
            | s <- bookStructures
            ]
        , testCase "the dispatcher reads every book witness" $
            mapM_ checkDispatch bookStructures
        , testCase "the role generator covers structures 1..37" $
            sort (map fst roleGenerators) @?= [1 .. 37]
        , testCase "the 4.2-4.6 sets are the keys of the combination tables" $ do
            sort bookPrefixes @?= sort (map fst book14)
            sort bookSuperfixes @?= sort (map fst book8)
            sort bookSubfixes @?= sort (map fst book9)
            sort bookPostfixes @?= sort (map fst book16)
        , testProperty
            "a Wylie word is read iff the roles are a legal composition"
            propBook
        , testProperty
            "a Unicode word is read iff the roles are a legal composition"
            propBookUnicode
        , testProperty
            "a Wylie and a Unicode reading share one normalized AST"
            propCrossScript
        , testCase "the a-chen carrier normalizes across the scripts" $
            crossScriptPair "i" "ཨི"
        ]

-- | The parser of a structure by its book number. The list is the one place
-- where the table and the grammar's names meet.
parsers :: [(Int, SpellParser TibetanSyllable)]
parsers =
    [ (1, pStructure1)
    , (2, pStructure2)
    , (3, pStructure3)
    , (4, pStructure4)
    , (5, pStructure5)
    , (6, pStructure6)
    , (7, pStructure7)
    , (8, pStructure8)
    , (9, pStructure9)
    , (10, pStructure10)
    , (11, pStructure11)
    , (12, pStructure12)
    , (13, pStructure13)
    , (14, pStructure14)
    , (15, pStructure15)
    , (16, pStructure16)
    , (17, pStructure17)
    , (18, pStructure18)
    , (19, pStructure19)
    , (20, pStructure20)
    , (21, pStructure21)
    , (22, pStructure22)
    , (23, pStructure23)
    , (24, pStructure24)
    , (25, pStructure25)
    , (26, pStructure26)
    , (27, pStructure27)
    , (28, pStructure28)
    , (29, pStructure29)
    , (30, pStructure30)
    , (31, pStructure31)
    , (32, pStructure32)
    , (33, pStructure33)
    , (34, pStructure34)
    , (35, pStructure35)
    , (36, pStructure36)
    , (37, pStructure37)
    ]

parserFor :: Int -> SpellParser TibetanSyllable
parserFor n = case lookup n parsers of
    Just p -> p
    Nothing -> error ("no parser for book structure " <> show n)

-- | A structure must read its witness whole, and the stacks must carry the
-- roles the book's rule names.
checkWitness :: BookStructure -> IO ()
checkWitness s =
    case parsed (parserFor (bookNumber s)) (bookWitness s) of
        Left err -> assertFailure (failed s err)
        Right (raws, roles) -> do
            T.concat raws @?= bookWitness s
            roles @?= bookRoles s

-- | The dispatcher must read the witness too; which structure wins is the
-- tie-break's business, not this table's.
checkDispatch :: BookStructure -> IO ()
checkDispatch s =
    case parsed pStructure (bookWitness s) of
        Left err -> assertFailure (failed s err)
        Right (raws, _) -> T.concat raws @?= bookWitness s

failed :: BookStructure -> Text -> String
failed s err =
    unwords
        [ "structure"
        , show (bookNumber s)
        , "did not read"
        , show (bookWitness s)
        , "-"
        , T.unpack err
        ]

-- | Read the whole input with one structure and hand back the tokens it took
-- and the roles of the stacks it built.
parsed :: SpellParser TibetanSyllable -> Text -> Either Text ([Text], [Role])
parsed p input =
    fmap
        (\(tokens, stacks) -> (map tokenRaw (toList tokens), map stRole (toList stacks)))
        ( parseEither
            (runSpell ((,) <$> p <*> takeStacks <* liftP MP.eof))
            (fst (tokenizeUnicode input))
        )

-- | The same over a Wylie run: the role generator writes Wylie, where every
-- letter is linear and no glyph layout is guessed.
parsedW :: SpellParser TibetanSyllable -> Text -> Either Text ([Text], [Role])
parsedW p input =
    fmap
        (\(tokens, stacks) -> (map tokenRaw (toList tokens), map stRole (toList stacks)))
        ( parseEither
            (runSpell ((,) <$> p <*> takeStacks <* liftP MP.eof))
            (fst (tokenizeWylie input))
        )

-- * The book's letter sets (rules 4.1-4.17), copied from the book

--
-- This is the second instance of the data: the grammar's own copy lives in
-- 'Convert.Grammar.Parser' and 'Convert.Grammar.Superfix'. A drift between the
-- two is exactly what the property below catches.

bookRoots :: [Consonant]
bookRoots =
    [ Ck
    , Ckh
    , Cg
    , Cng
    , Cc
    , Cch
    , Cj
    , Cny
    , Ct
    , Cth
    , Cd
    , Cn
    , Cp
    , Cph
    , Cb
    , Cm
    , Cts
    , Ctsh
    , Cdz
    , Cw
    , Czh
    , Cz
    , C'
    , Cy
    , Cr
    , Cl
    , Csh
    , Cs
    , Ch
    , Ca
    ]

-- | The generator's root alphabet. The a-chen carries its own a and opens a
-- stack of its own (its Wylie spelling is the implicit vowel too), so it is the
-- one root the role generator leaves out; its forms are covered by
-- 'Test.Tokenizer.RoundTrip'.
generatorRoots :: [Consonant]
generatorRoots = filter (/= Ca) bookRoots

bookPrefixes :: [Consonant]
bookPrefixes = [Cg, Cd, Cb, Cm, C']

bookSuffixes :: [Consonant]
bookSuffixes = [Cg, Cng, Cd, Cn, Cb, Cm, C', Cr, Cl, Cs]

bookPostfixes :: [Consonant]
bookPostfixes = [Cd, Cs]

bookSuperfixes :: [Consonant]
bookSuperfixes = [Cr, Cl, Cs]

bookSubfixes :: [Consonant]
bookSubfixes = [Cw, Cy, Cr, Cl]

bookVowels :: [Vowel]
bookVowels = [Vi, Vu, Ve, Vo]

-- | Rule 4.8: the roots each superfix gates.
book8 :: [(Consonant, [Consonant])]
book8 =
    [ (Cr, [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cb, Cm, Cts, Cdz])
    , (Cl, [Ck, Cg, Cng, Cc, Cj, Ct, Cd, Cp, Cb, Ch])
    , (Cs, [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cp, Cb, Cm, Cts])
    ]

-- | Rule 4.9: the roots each subfix takes.
book9 :: [(Consonant, [Consonant])]
book9 =
    [ (Cw, [Ck, Ckh, Cg, Cny, Cd, Ctsh, Czh, Cz, Cr, Cl, Csh, Ch])
    , (Cy, [Ck, Ckh, Cg, Cp, Cph, Cb, Cm])
    , (Cr, [Ck, Ckh, Cg, Ct, Cth, Cd, Cp, Cph, Cb, Cm, Cs, Ch])
    , (Cl, [Ck, Cg, Cb, Cz, Cr, Cs])
    ]

-- | Rule 4.10: (superfix, root, subfix) rows.
book10 :: [(Consonant, Consonant, Consonant)]
book10 =
    [ (Cr, Ck, Cy)
    , (Cr, Cg, Cy)
    , (Cr, Cm, Cy)
    , (Cs, Ck, Cy)
    , (Cs, Ck, Cr)
    , (Cs, Cg, Cy)
    , (Cs, Cg, Cr)
    , (Cs, Cn, Cr)
    , (Cs, Cp, Cy)
    , (Cs, Cp, Cr)
    , (Cs, Cb, Cy)
    , (Cs, Cb, Cr)
    , (Cs, Cm, Cy)
    , (Cs, Cm, Cr)
    , (Cr, Cts, Cw)
    ]

-- | Rule 4.11: (prefix, superfix, roots).
book11 :: [(Consonant, Consonant, [Consonant])]
book11 =
    [ (Cb, Cr, [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cts, Cdz])
    , (Cb, Cl, [Ct, Cd])
    , (Cb, Cs, [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cts])
    ]

-- | Rule 4.12: (prefix, subfix, roots).
book12 :: [(Consonant, Consonant, [Consonant])]
book12 =
    [ (Cd, Cy, [Ck, Cg, Cp, Cb, Cm])
    , (Cd, Cr, [Ck, Cg, Cp, Cb])
    , (Cb, Cy, [Ck, Cg])
    , (Cb, Cr, [Ck, Cg, Cs])
    , (Cb, Cl, [Ck, Cz, Cr, Cs])
    , (Cm, Cy, [Ckh, Cg])
    , (Cm, Cr, [Ckh, Cg])
    , (C', Cy, [Ckh, Cg, Cph, Cb])
    , (C', Cr, [Ckh, Cg, Cd, Cph, Cb])
    ]

-- | Rule 4.13: (prefix, superfix, subfix, roots).
book13 :: [(Consonant, Consonant, Consonant, [Consonant])]
book13 =
    [ (Cb, Cr, Cy, [Ck, Cg])
    , (Cb, Cs, Cr, [Ck, Cg])
    , (Cb, Cs, Cy, [Ck, Cg])
    ]

-- | Rule 4.14: the roots each prefix takes.
book14 :: [(Consonant, [Consonant])]
book14 =
    [ (Cg, [Cc, Cny, Ct, Cd, Cn, Cts, Czh, Cz, Cy, Csh, Cs])
    , (Cd, [Ck, Cg, Cng, Cp, Cb, Cm])
    , (Cb, [Ck, Cg, Cc, Ct, Cd, Cts, Czh, Cz, Csh, Cs])
    , (Cm, [Ckh, Cg, Cng, Cch, Cj, Cny, Cth, Cd, Cn, Ctsh, Cdz])
    , (C', [Ckh, Cg, Cch, Cj, Cth, Cd, Cph, Cb, Ctsh, Cdz])
    ]

-- | Rule 4.16: the suffix group each postfix takes.
book16 :: [(Consonant, [Consonant])]
book16 =
    [ (Cd, [Cn, Cr, Cl])
    , (Cs, [Cg, Cng, Cb, Cm])
    ]

-- | Rule 4.17: (root, first subfix, second subfix).
book17 :: [(Consonant, Consonant, Consonant)]
book17 =
    [ (Cg, Cr, Cw)
    , (Cd, Cr, Cw)
    , (Cph, Cy, Cw)
    ]

-- * The role generator

--
-- shortcut: the generator writes Wylie only and mutates the root to CR; the
-- a-chen and the Tibetan layout stay outside this slice (the witness table
-- covers them), revisit when the oracle checks both scripts.

-- | A letter the generator may mutate. The slot is kept with the chosen
-- letter, so an illegal case is the same word with its root replaced by a
-- letter no book slot allows.
data SlotName = SPrefix | SSuperfix | SRoot | SSubfix | SSuffix | SPostfix
    deriving (Eq, Show)

-- | How the letter is written in the glyphs: full, or subjoined below the
-- stack it belongs to. Wylie writes every letter full, so the form only
-- matters to 'renderUnicode'.
data Form = Full | Joined
    deriving (Eq, Show)

data Tagged = Tagged
    { tagSlot :: SlotName
    , tagLetter :: Consonant
    , tagForm :: Form
    }
    deriving (Show)

-- | One piece of the Wylie word. 'PImplicitA' is the a Wylie writes for a root
-- that has no written vowel: the parser reads it as 'VowelImplicit', and the
-- book's prose (1.2) is what makes it part of the syllable.
data Piece = PCons Tagged | PVowel Vowel | PImplicitA | PDot
    deriving (Show)

pick :: [a] -> Gen a
pick = Gen.elem . NE.fromList

consonantPiece :: SlotName -> Form -> [Consonant] -> Gen Piece
consonantPiece slot form allowed = do
    c <- pick allowed
    pure (PCons (Tagged slot c form))

prefix, superfix, subfix, postfix :: Consonant -> Gen Piece
prefix c = consonantPiece SPrefix Full [c]
superfix c = consonantPiece SSuperfix Full [c]
subfix c = consonantPiece SSubfix Joined [c]
postfix c = consonantPiece SPostfix Full [c]

root :: [Consonant] -> Gen Piece
root = consonantPiece SRoot Full

-- | A root that is written below a superfix (or below the h of 4.18).
rootJoined :: [Consonant] -> Gen Piece
rootJoined = consonantPiece SRoot Joined

suffix :: [Consonant] -> Gen Piece
suffix = consonantPiece SSuffix Full

optionalVowel :: Gen [Piece]
optionalVowel = Gen.list (Range.inclusive (0, 1)) (PVowel <$> pick bookVowels)

requiredVowel :: Gen [Piece]
requiredVowel = (: []) . PVowel <$> pick bookVowels

-- | The Wylie a for a root with no written vowel.
implicit :: Bool -> [Piece]
implicit hasWritten = if hasWritten then [] else [PImplicitA]

renderPieces :: [Piece] -> Text
renderPieces = T.concat . map one
    where
        one (PCons t) = wylieConsonant (tagLetter t)
        one (PVowel v) = wylieVowel v
        one PImplicitA = "a"
        one PDot = "."

-- | The same pieces in Tibetan glyphs. The implicit a is not written (the
-- parser adds 'VowelImplicit'), the Wylie dot has no glyph, and a joined
-- letter is its subjoined sign.
renderUnicode :: [Piece] -> Text
renderUnicode = T.concat . map one
    where
        one (PCons t) = case tagForm t of
            Full -> glyph (TcConsonant (tagLetter t))
            Joined -> joined (tagLetter t)
        one (PVowel v) = glyph (TcVowel v)
        one PImplicitA = ""
        one PDot = ""
        glyph c = maybe "" id (unicodeOf c)
        joined c = maybe "" (glyph . TcSubConsonant) (toSubjoined c)

-- | The Unicode reading of the same roles. The differences are the script's
-- stack grouping: h+ph is one stack in the glyphs, and the a-chung trailer
-- ng/m stays inside the a-chung's stack.
unicodeRoles :: Int -> [Role] -> [Role]
unicodeRoles n wylieRoles = case n of
    26 -> [RoleRoot]
    27 -> [RoleRoot, RoleSuffix]
    37 -> [RoleRoot, RoleSuffix]
    _ | n >= 28 && n <= 31 -> [RoleRoot, RoleRoot]
    _ -> wylieRoles

-- | The base of a structure: its pieces and whether a vowel sign was written.
-- A base with no written vowel takes an implicit a when the word is finished.
base1
    , base2
    , base3
    , base4
    , base5
    , base6
    , base7
    , base8
    , base25
    , base26 ::
        Gen ([Piece], Bool)
base1 = do
    r <- root generatorRoots
    v <- optionalVowel
    pure ([r] ++ v, not (null v))
base2 = do
    (sf, roots) <- pick book8
    s <- superfix sf
    r <- rootJoined roots
    v <- optionalVowel
    pure ([s, r] ++ v, not (null v))
base3 = do
    (sb, roots) <- pick book9
    r <- root roots
    b <- subfix sb
    v <- optionalVowel
    pure ([r, b] ++ v, not (null v))
base4 = do
    (sf, r0, sb) <- pick book10
    s <- superfix sf
    r <- rootJoined [r0]
    b <- subfix sb
    v <- optionalVowel
    pure ([s, r, b] ++ v, not (null v))
base5 = do
    (p, sf, roots) <- pick book11
    pr <- prefix p
    s <- superfix sf
    r <- rootJoined roots
    v <- optionalVowel
    pure ([pr, s, r] ++ v, not (null v))
base6 = do
    (p, sb, roots) <- pick book12
    pr <- prefix p
    r <- root roots
    b <- subfix sb
    v <- optionalVowel
    pure ([pr, r, b] ++ v, not (null v))
base7 = do
    (p, sf, sb, roots) <- pick book13
    pr <- prefix p
    s <- superfix sf
    r <- rootJoined roots
    b <- subfix sb
    v <- optionalVowel
    pure ([pr, s, r, b] ++ v, not (null v))
base8 = do
    (p, roots) <- pick book14
    rc <- pick roots
    pr <- prefix p
    -- The dot forces the prefix reading when the root would otherwise subjoin
    -- to the prefix letter: gy is a stack (4.17), g.y is prefix g + root y
    -- (4.14). The dot is Wylie syntax, not a book symbol.
    let dot = [PDot | rc `elem` bookSubfixes]
    r <- root [rc]
    v <- optionalVowel
    pure ([pr] ++ dot ++ [r] ++ v, not (null v))
base25 = do
    (r0, s1, s2) <- pick book17
    r <- root [r0]
    b1 <- subfix s1
    b2 <- subfix s2
    v <- optionalVowel
    pure ([r, b1, b2] ++ v, not (null v))
base26 = do
    h <- root [Ch]
    ph <- rootJoined [Cph]
    v <- optionalVowel
    pure ([h, ph] ++ v, not (null v))

-- | A suffix and postfix that rule 4.16 allows together.
suffixPostfix :: Gen (Piece, Piece)
suffixPostfix = do
    (po, sufs) <- pick book16
    sfx <- suffix sufs
    pof <- postfix po
    pure (sfx, pof)

-- | The a-chung and its trailer, with the number of root stacks it adds: a
-- vowel stays in the a-chung's stack, while ng/m is a full letter in Wylie and
-- starts a stack of its own (in Tibetan it is subjoined and stays inside).
aChungRoot :: Gen ([Piece], Int)
aChungRoot = do
    a <- root [C']
    (trailer, extra) <-
        pick
            ( [(PVowel v, 1) | v <- bookVowels]
                ++ [ (PCons (Tagged SRoot Cng Joined), 2)
                   , (PCons (Tagged SRoot Cm Joined), 2)
                   ]
            )
    pure ([a, trailer], extra)

-- | The a-chung as the word's last suffix.
aChungSuffix :: Gen Piece
aChungSuffix = suffix [C']

roleGenerators :: [(Int, Gen (Int, [Piece], [Role]))]
roleGenerators =
    [ (1, gen1)
    , (2, gen2)
    , (3, gen3)
    , (4, gen4)
    , (5, gen5)
    , (6, gen6)
    , (7, gen7)
    , (8, gen8)
    , (9, gen9)
    , (10, gen10)
    , (11, gen11)
    , (12, gen12)
    , (13, gen13)
    , (14, gen14)
    , (15, gen15)
    , (16, gen16)
    , (17, gen17)
    , (18, gen18)
    , (19, gen19)
    , (20, gen20)
    , (21, gen21)
    , (22, gen22)
    , (23, gen23)
    , (24, gen24)
    , (25, gen25)
    , (26, gen26)
    , (27, gen27)
    , (28, gen28)
    , (29, gen29)
    , (30, gen30)
    , (31, gen31)
    , (32, gen32)
    , (33, gen33)
    , (34, gen34)
    , (35, gen35)
    , (36, gen36)
    , (37, gen37)
    ]

withBase ::
    Int
    -> [Role]
    -> Gen ([Piece], Bool)
    -> Gen (Int, [Piece], [Role])
withBase n roles base = do
    (pieces, hasVowel) <- base
    pure (n, pieces ++ implicit hasVowel, roles)

withSuffix ::
    Int
    -> [Role]
    -> Gen ([Piece], Bool)
    -> Gen (Int, [Piece], [Role])
withSuffix n roles base = do
    (pieces, hasVowel) <- base
    sfx <- suffix bookSuffixes
    pure (n, pieces ++ implicit hasVowel ++ [sfx], roles)

withPostfix ::
    Int
    -> [Role]
    -> Gen ([Piece], Bool)
    -> Gen (Int, [Piece], [Role])
withPostfix n roles base = do
    (pieces, hasVowel) <- base
    (sfx, pof) <- suffixPostfix
    pure (n, pieces ++ implicit hasVowel ++ [sfx, pof], roles)

gen1 :: Gen (Int, [Piece], [Role])
gen1 = withBase 1 [RoleRoot] base1

gen2 :: Gen (Int, [Piece], [Role])
gen2 = withBase 2 [RoleRoot] base2

gen3 :: Gen (Int, [Piece], [Role])
gen3 = withBase 3 [RoleRoot] base3

gen4 :: Gen (Int, [Piece], [Role])
gen4 = withBase 4 [RoleRoot] base4

gen5 :: Gen (Int, [Piece], [Role])
gen5 = withBase 5 [RolePrefix, RoleRoot] base5

gen6 :: Gen (Int, [Piece], [Role])
gen6 = withBase 6 [RolePrefix, RoleRoot] base6

gen7 :: Gen (Int, [Piece], [Role])
gen7 = withBase 7 [RolePrefix, RoleRoot] base7

gen8 :: Gen (Int, [Piece], [Role])
gen8 = withBase 8 [RolePrefix, RoleRoot] base8

gen9 :: Gen (Int, [Piece], [Role])
gen9 = withSuffix 9 [RolePrefix, RoleRoot, RoleSuffix] base8

gen10 :: Gen (Int, [Piece], [Role])
gen10 = withSuffix 10 [RolePrefix, RoleRoot, RoleSuffix] base5

gen11 :: Gen (Int, [Piece], [Role])
gen11 = withSuffix 11 [RolePrefix, RoleRoot, RoleSuffix] base6

gen12 :: Gen (Int, [Piece], [Role])
gen12 = withSuffix 12 [RolePrefix, RoleRoot, RoleSuffix] base7

gen13 :: Gen (Int, [Piece], [Role])
gen13 = withPostfix 13 [RolePrefix, RoleRoot, RoleSuffix, RolePostfix] base8

gen14 :: Gen (Int, [Piece], [Role])
gen14 = withPostfix 14 [RolePrefix, RoleRoot, RoleSuffix, RolePostfix] base5

gen15 :: Gen (Int, [Piece], [Role])
gen15 = withPostfix 15 [RolePrefix, RoleRoot, RoleSuffix, RolePostfix] base6

gen16 :: Gen (Int, [Piece], [Role])
gen16 = withPostfix 16 [RolePrefix, RoleRoot, RoleSuffix, RolePostfix] base7

gen17 :: Gen (Int, [Piece], [Role])
gen17 = withSuffix 17 [RoleRoot, RoleSuffix] base1

gen18 :: Gen (Int, [Piece], [Role])
gen18 = withSuffix 18 [RoleRoot, RoleSuffix] base2

gen19 :: Gen (Int, [Piece], [Role])
gen19 = withSuffix 19 [RoleRoot, RoleSuffix] base3

gen20 :: Gen (Int, [Piece], [Role])
gen20 = withSuffix 20 [RoleRoot, RoleSuffix] base4

gen21 :: Gen (Int, [Piece], [Role])
gen21 = withPostfix 21 [RoleRoot, RoleSuffix, RolePostfix] base1

gen22 :: Gen (Int, [Piece], [Role])
gen22 = withPostfix 22 [RoleRoot, RoleSuffix, RolePostfix] base2

gen23 :: Gen (Int, [Piece], [Role])
gen23 = withPostfix 23 [RoleRoot, RoleSuffix, RolePostfix] base3

gen24 :: Gen (Int, [Piece], [Role])
gen24 = withPostfix 24 [RoleRoot, RoleSuffix, RolePostfix] base4

gen25 :: Gen (Int, [Piece], [Role])
gen25 = withBase 25 [RoleRoot] base25

-- | 4.18/4.19 in Wylie: @hph@ is two full letters, so the subroot @ph@ is a
-- root stack of its own; in Tibetan the subjoined sign keeps it in ཧ's stack
-- and the table's witness gives one 'RoleRoot'. The split is the documented
-- cross-script difference, not a spelling difference.
gen26 :: Gen (Int, [Piece], [Role])
gen26 = withBase 26 [RoleRoot, RoleRoot] base26

gen27 :: Gen (Int, [Piece], [Role])
gen27 = withSuffix 27 [RoleRoot, RoleRoot, RoleSuffix] base26

-- | The 4.20 forms. The base of 28-31 carries a written vowel, as the book's
-- G_i require; the a-chung is the second root and takes a vowel or ng/m.
gen28 :: Gen (Int, [Piece], [Role])
gen28 = aChungAfter 28 [RoleRoot] base1Required

gen29 :: Gen (Int, [Piece], [Role])
gen29 = aChungAfter 29 [RoleRoot] base2Required

gen30 :: Gen (Int, [Piece], [Role])
gen30 = aChungAfter 30 [RoleRoot] base3Required

gen31 :: Gen (Int, [Piece], [Role])
gen31 = aChungAfter 31 [RoleRoot] base4Required

base1Required, base2Required, base3Required, base4Required :: Gen [Piece]
base1Required = do
    r <- root generatorRoots
    v <- requiredVowel
    pure ([r] ++ v)
base2Required = do
    (sf, roots) <- pick book8
    s <- superfix sf
    r <- rootJoined roots
    v <- requiredVowel
    pure ([s, r] ++ v)
base3Required = do
    (sb, roots) <- pick book9
    r <- root roots
    b <- subfix sb
    v <- requiredVowel
    pure ([r, b] ++ v)
base4Required = do
    (sf, r0, sb) <- pick book10
    s <- superfix sf
    r <- rootJoined [r0]
    b <- subfix sb
    v <- requiredVowel
    pure ([s, r, b] ++ v)

aChungAfter :: Int -> [Role] -> Gen [Piece] -> Gen (Int, [Piece], [Role])
aChungAfter n roles base = do
    pieces <- base
    (a, extra) <- aChungRoot
    pure (n, pieces ++ a, roles ++ replicate extra RoleRoot)

-- | The four 4.20 forms over a prefix base: the a-chung stands bare, so it is
-- the word's last suffix (our reading of 4.20 where the book leaves it open).
gen32 :: Gen (Int, [Piece], [Role])
gen32 = aChungSuffixAfter 32 [RolePrefix, RoleRoot, RoleSuffix] base5

gen33 :: Gen (Int, [Piece], [Role])
gen33 = aChungSuffixAfter 33 [RolePrefix, RoleRoot, RoleSuffix] base6

gen34 :: Gen (Int, [Piece], [Role])
gen34 = aChungSuffixAfter 34 [RolePrefix, RoleRoot, RoleSuffix] base7

gen35 :: Gen (Int, [Piece], [Role])
gen35 = aChungSuffixAfter 35 [RolePrefix, RoleRoot, RoleSuffix] base8

gen36 :: Gen (Int, [Piece], [Role])
gen36 = aChungSuffixAfter 36 [RoleRoot, RoleSuffix] base25

gen37 :: Gen (Int, [Piece], [Role])
gen37 = do
    pieces <- base26Required
    a <- aChungSuffix
    pure (37, pieces ++ [a], [RoleRoot, RoleRoot, RoleSuffix])

base26Required :: Gen [Piece]
base26Required = do
    h <- root [Ch]
    ph <- rootJoined [Cph]
    v <- requiredVowel
    pure ([h, ph] ++ v)

aChungSuffixAfter ::
    Int
    -> [Role]
    -> Gen ([Piece], Bool)
    -> Gen (Int, [Piece], [Role])
aChungSuffixAfter n roles base = do
    (pieces, hasVowel) <- base
    a <- aChungSuffix
    pure (n, pieces ++ implicit hasVowel ++ [a], roles)

-- | A legal case, or the same case with the root taken outside its own slot's
-- set. The mutation is the "book says no" half of the property.
-- | A random legal book syllable in Wylie. The structural fuzz uses it so its
-- documents reach all 37 structures, not just the few the letter-run generator
-- happens to hit.
bookSyllable :: Gen Text
bookSyllable = do
    g <- pick (map snd roleGenerators)
    (_, pieces, _) <- g
    pure (renderPieces pieces)

genCase :: Gen (Int, [Piece], [Role], Bool)
genCase = do
    g <- pick (map snd roleGenerators)
    (n, pieces, roles) <- g
    legal <- pick [True, False]
    let pieces' = if legal then pieces else mutateRoot pieces
    pure (n, pieces', roles, legal)

mutateRoot :: [Piece] -> [Piece]
mutateRoot = go
    where
        go [] = []
        go (PCons t : ps)
            | tagSlot t == SRoot = PCons t{tagLetter = CR} : ps
            | otherwise = PCons t : go ps
        go (p : ps) = p : go ps

-- * The cross-script AST shape

-- | A stack with the script-specific facts removed: subjoined signs are full
-- letters, the a-chen carrier is not a letter, and the stack grouping of a
-- subjoined second root is normalized by 'mergeRoots'. Tokens, spans and token
-- sources are not part of the shape.
--
-- The caret and the document-level a-chen split (Wylie @ae@ is two syllables,
-- Tibetan ཨཨེ one) stay outside: the first is a sign both tokenizers carry, the
-- second is conversion policy, not one syllable's AST.
data StackShape = StackShape
    { shapeRole :: Role
    , shapeLetters :: [Consonant]
    , shapeVowel :: VowelShape
    , shapeFinals :: [FinalMark]
    }
    deriving (Eq, Show)

data VowelShape = NoVowel | ImplicitVowel | WrittenVowel [Vowel]
    deriving (Eq, Show)

shape :: [Stack] -> [StackShape]
shape = mergeRoots . map stackShape

stackShape :: Stack -> StackShape
stackShape st =
    StackShape
        { shapeRole = stRole st
        , shapeLetters =
            [ c
            | tok <-
                maybe [] pure (stSuperfix st)
                    <> maybe [] pure (stRoot st)
                    <> subfixTokens (stSubfixes st)
            , Just c <- [consonantOf (tokenCanonical tok)]
            , not (dropCarrier st tok)
            ]
        , shapeVowel = vowelOf (stVowel st)
        , shapeFinals =
            [ f
            | tok <- finalsTokens (stFinals st)
            , TcFinal f <- [tokenCanonical tok]
            ]
        }

consonantOf :: TokenCanonical -> Maybe Consonant
consonantOf (TcConsonant c) = Just c
consonantOf (TcSubConsonant sc) = Just (fromSubjoined sc)
consonantOf _ = Nothing

subfixTokens :: Subfixes -> [Token]
subfixTokens NoSubfix = []
subfixTokens (OneSubfix a) = [a]
subfixTokens (TwoSubfix a b) = [a, b]
subfixTokens (ThreeSubfix a b c) = [a, b, c]

-- | The a-chen that only carries a written vowel is the vowel's carrier, not a
-- letter of its own (Wylie @i@ and Tibetan ཨི are the same shape).
dropCarrier :: Stack -> Token -> Bool
dropCarrier st tok =
    tokenCanonical tok == TcConsonant Ca
        && stSuperfix st == Nothing
        && stSubfixes st == NoSubfix
        && writtenVowel (stVowel st)

vowelOf :: Maybe VowelSlot -> VowelShape
vowelOf Nothing = NoVowel
vowelOf (Just VowelImplicit) = ImplicitVowel
vowelOf (Just (VowelWritten tok rest)) =
    WrittenVowel [v | t <- tok : rest, TcVowel v <- [tokenCanonical t]]

writtenVowel :: Maybe VowelSlot -> Bool
writtenVowel (Just (VowelWritten _ _)) = True
writtenVowel _ = False

-- | A second root written below the first is one stack in the glyphs and two
-- in Wylie: 4.18's @h+ph@ and 4.20's a-chung @+ ng/m@. Only those pairs merge;
-- any other second root is a root of its own in both scripts.
mergeRoots :: [StackShape] -> [StackShape]
mergeRoots (a : b : rest)
    | subjoinedRoots a b
    , null (shapeFinals a)
    , null (shapeFinals b) =
        mergeRoots
            ( a
                { shapeLetters = shapeLetters a <> shapeLetters b
                , shapeVowel = mergeVowel (shapeVowel a) (shapeVowel b)
                , shapeFinals = shapeFinals b
                }
                : rest
            )
mergeRoots (a : rest) = a : mergeRoots rest
mergeRoots [] = []

subjoinedRoots :: StackShape -> StackShape -> Bool
subjoinedRoots a b = case (shapeLetters a, shapeLetters b) of
    ([Ch], [Cph]) -> True
    ([C'], [Cng]) -> True
    ([C'], [Cm]) -> True
    _ -> False

mergeVowel :: VowelShape -> VowelShape -> VowelShape
mergeVowel ImplicitVowel v = v
mergeVowel v _ = v

stacksW :: SpellParser TibetanSyllable -> Text -> Either Text [Stack]
stacksW p input =
    fmap (toList . snd) $
        parseEither
            (runSpell ((,) <$> p <*> takeStacks <* liftP MP.eof))
            (fst (tokenizeWylie input))

stacksU :: SpellParser TibetanSyllable -> Text -> Either Text [Stack]
stacksU p input =
    fmap (toList . snd) $
        parseEither
            (runSpell ((,) <$> p <*> takeStacks <* liftP MP.eof))
            (fst (tokenizeUnicode input))

genLegalCase :: Gen (Int, [Piece])
genLegalCase = do
    g <- pick (map snd roleGenerators)
    (n, pieces, _) <- g
    pure (n, pieces)

propCrossScript :: Property ()
propCrossScript = do
    c <- gen genLegalCase
    assert $
        P.lam
            ( \value ->
                case checkCrossScript value of
                    [] -> P.pass
                    reasons -> P.fail (unlines reasons)
            )
            P..$ ("case", c)

crossScriptPair :: Text -> Text -> Assertion
crossScriptPair wylie unicode =
    case (stacksW pStructure wylie, stacksU pStructure unicode) of
        (Right ws, Right us) -> shape ws @?= shape us
        (Left err, _) -> assertFailure ("Wylie parse failed: " <> T.unpack err)
        (_, Left err) -> assertFailure ("Unicode parse failed: " <> T.unpack err)

checkCrossScript :: (Int, [Piece]) -> [String]
checkCrossScript (n, pieces) =
    case (stacksW parser (renderPieces pieces), stacksU parser (renderUnicode pieces)) of
        (Left err, _) -> ["Wylie parse failed: " <> T.unpack err]
        (_, Left err) -> ["Unicode parse failed: " <> T.unpack err]
        (Right ws, Right us) ->
            let sw = shape ws
                su = shape us
             in [ "shapes differ: " <> show sw <> " /= " <> show su
                | sw /= su
                ]
    where
        parser = parserFor n

propBook :: Property ()
propBook = propertyOf checkWylieCase

propBookUnicode :: Property ()
propBookUnicode = propertyOf checkUnicodeCase

propertyOf :: ((Int, [Piece], [Role], Bool) -> [String]) -> Property ()
propertyOf check = do
    c <- gen genCase
    assert $
        P.lam
            ( \value ->
                case check value of
                    [] -> P.pass
                    reasons -> P.fail (unlines reasons)
            )
            P..$ ("case", c)

checkWylieCase :: (Int, [Piece], [Role], Bool) -> [String]
checkWylieCase (n, pieces, roles, legal) =
    let word = renderPieces pieces
     in checkParsed
            "Wylie"
            n
            (T.filter (/= '.') word)
            roles
            legal
            (parsedW (parserFor n) word)

checkUnicodeCase :: (Int, [Piece], [Role], Bool) -> [String]
checkUnicodeCase (n, pieces, roles, legal) =
    let word = renderUnicode pieces
     in checkParsed
            "Unicode"
            n
            word
            (unicodeRoles n roles)
            legal
            (parsed (parserFor n) word)

checkParsed ::
    Text
    -> Int
    -> Text
    -> [Role]
    -> Bool
    -> Either Text ([Text], [Role])
    -> [String]
checkParsed script n expected roles legal result =
    case result of
        Left err
            | legal -> [bad "did not read a legal word" (T.unpack err)]
            | otherwise -> []
        Right (raws, got)
            | not legal -> [bad "read a word the book rejects" ""]
            | T.concat raws /= expected ->
                [bad "did not read the word whole" (show (T.concat raws))]
            | got /= roles -> [bad "roles" (show got <> " /= " <> show roles)]
            | otherwise -> []
    where
        bad what extra =
            unwords
                [ T.unpack script
                , "structure"
                , show n
                , what
                , show expected
                , extra
                ]
