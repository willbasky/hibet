-- Ожидания: книга (Nyima Tashi) — таблица 37 структур и их достижимость.

-- | Wave 10, remaining step 1: the book's 37 syllable structures as data, and
-- the reachability check that each of them has a word it reads.
--
-- The book states rules 4.1-4.20, not a list of 37 rows: the rows are the
-- compositions the grammar uses ('Convert.Grammar.Structure'). This table
-- writes those compositions down once, independently of the parsers, with a
-- witness drawn from the book's own letter sets. The witness is what a
-- structure must accept in full, and the roles are what the book's rule puts
-- into the stacks.
--
-- The oracle property - "accepted iff the book accepts" - is not here yet; it
-- needs the letter sets of rules 4.2-4.14 as data too and is the next slice.
module Test.Grammar.Book (tests) where

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
    , TibetanSyllable
    )
import Convert.Token (tokenRaw)
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
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
