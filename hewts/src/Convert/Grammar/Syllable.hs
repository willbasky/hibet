-- | A parsed Tibetan syllable: the tokens of one syllable, each marked with the
-- place it occupies in the syllable.
--
-- The vocabulary is the book's own, from "Research on Tibetan Spelling Formal
-- Language and Automata with Application" (Nyima Tashi, Science Press + Springer,
-- 2019), section 4.1, Definitions 4.1-4.8. Our 37 structures come from the same
-- book (its "Tibetan spelling grammar 4.1-4.20", section 4.2), so the marks use the
-- terms the rules themselves use.
--
-- The mark lives in the list rather than in 'Token': a letter is superfix or
-- subfix because of where it stands, not because of what it is - the same @r@ is a
-- root in one syllable and a subfix in another. Leaving 'Token' alone also leaves
-- its invariants alone (input coverage).
--
-- This module also holds the nested syllable of REFACTOR-AST: 'Stack' with its
-- 'Role', 'Subfixes', 'VowelSlot' and 'Finals'. The flat 'TibetanSyllable' above
-- is the grammar's own product until step 4; 'stacksOf' bridges it into the
-- nested form, and since step 3 everything outside the grammar reads that form.
module Convert.Grammar.Syllable
    ( Position (..)
    , TibetanSyllable
    , mark
    , markS
    , subfixMarks
    , caretMark
    , Role (..)
    , Subfixes (..)
    , VowelSlot (..)
    , Finals
    , emptyFinals
    , snocFinal
    , finalsTokens
    , Syllable (..)
    , Stack (..)
    , stackTokens
    , stacksOf
    , flatten
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Parser (Parser, SpellParser, liftP)
import Convert.Token
    ( FinalMark (..)
    , Span
    , Token
    , TokenCanonical (..)
    , finalSlot
    , tokenCanonical
    )
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (catMaybes, isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | The place a character occupies in a Tibetan syllable.
--
-- 'Final' is ours rather than the book's: the book files final marks with the
-- other signs (Def 4.9) instead of giving them a place of their own, but we
-- need to tell them apart - a final that does not fit the closing chain is an
-- error, and the order of assembly depends on it.
data Position
    = Root
    | Prefix
    | Superfix
    | Subfix
    | Suffix
    | Postfix
    | Vowel
    | Final
    | ImplicitVowel
    | -- | A mark the grammar took and the window named: it stays in the parse
      -- and in the run, but it prints nothing, exactly as the second caret of
      -- @g^r^a@ does. Nothing is lost from the token stream - the finding
      -- quotes the run, so the mark is still there to be read.
      Eaten
    deriving (Show, Eq, Ord)

-- | A Tibetan syllable (Def 4.10) as a flat list of marked tokens.
--
-- This is the grammar's own product, and it stays so until REFACTOR-AST
-- step 4: the sentence builds the nested 'Stack' below from it with 'stacksOf'
-- in the meantime, so there is one reading of the marks and no second parse. A 'Seq' rather than a list because the syllable is built by
-- appending pieces (a prefix, a root, a subfix, a vowel) and read from both
-- ends by the legality rules of wave 3.4. Be aware that this buys clarity
-- more than speed: a syllable holds one to six letters, and at that size 'Seq'
-- and a list append in the same handful of steps. The real cost in this layer
-- is elsewhere - see 'Convert.Grammar.Structure.pStructure', which probes all 37
-- structures in lookahead, so every syllable is parsed 37 times over.
type TibetanSyllable = Seq (Position, Token)

-- | Give a parser a position, so it yields a one-element syllable. Marking at the
-- point of binding keeps the token parsers themselves unchanged: they parse
-- letters, they know nothing about syllables.
mark :: Position -> Parser a -> Parser (Seq (Position, a))
mark position parser = do
    value <- parser
    pure (Seq.singleton (position, value))

-- | 'mark' in the spell parser: the constraint windows that run statefully peg
-- their tokens the same way, then write whatever rule fires at the window.
markS :: Position -> Parser a -> SpellParser (Seq (Position, a))
markS position parser = liftP (mark position parser)

-- | The subfix letters of one stack, marked in order.
subfixMarks :: [Token] -> TibetanSyllable
subfixMarks = Seq.fromList . map (Subfix,)

-- | The one caret that survives a subjoining scan, marked 'Final', or nothing
-- when the scan kept no caret.
caretMark :: Maybe Token -> TibetanSyllable
caretMark = maybe mempty (Seq.singleton . (Final,))

-- | The role a stack plays in its syllable (REFACTOR-AST §4). Needed for two
-- printing decisions only: the implicit @a@ and the stack separator.
data Role
    = RolePrefix
    | RoleRoot
    | RoleSuffix
    | RolePostfix
    deriving (Show, Eq, Ord)

-- | The subjoined letters of a stack, without a hole: no "second one but no
-- first". The book allows two and 'pSubjoinRun' stops there; a third comes
-- from the Sanskrit transliteration of the corpora (@གྨྲྸ@) and from the
-- forced joins of the Wylie fallback (@g+m+r+a@). The Unicode @སྒྲྭ@ is not
-- a third subfix: it is a superfix and root plus two subfixes, and today's
-- generic parse loses that superfix (REFACTOR-AST §8 A3).
data Subfixes
    = NoSubfix
    | OneSubfix Token
    | TwoSubfix Token Token
    | ThreeSubfix Token Token Token
    deriving (Show, Eq)

-- | The vowel slot of a stack. @VowelWritten@ carries every written sign of
-- the slot: the long letters are two tokens (@ཱ@ plus the short sign, @I@ as
-- two Wylie letters), and a forced join may add another sign (@bru+e@ is
-- བྲེུ). @VowelImplicit@ is the implicit @a@ - a fact, not a token (Wylie
-- writes it with the letter @a@, Unicode writes nothing).
data VowelSlot
    = VowelWritten Token [Token]
    | VowelImplicit
    deriving (Show, Eq)

-- | The closing chain of a stack: eight slots (see 'Convert.Token.finalSlot'),
-- each holding at most one mark, in increasing slot order. The constructor is
-- hidden: 'emptyFinals' and 'snocFinal' are the only way in.
data Finals = Finals (Seq (Maybe Token))
    deriving (Show, Eq)

emptyFinals :: Finals
emptyFinals = Finals (Seq.replicate 8 Nothing)

-- | Add a final mark to its slot; refuses a mark whose slot is already filled
-- or whose chain has passed it. Such a mark is 'Eaten' in today's parse, so a
-- refusal here means the bridge met a shape the grammar did not mark that way.
snocFinal :: Token -> Finals -> Maybe Finals
snocFinal tok (Finals slots) = case tokenCanonical tok of
    TcFinal mark -> do
        slot <- finalSlot mark
        let i = slot - 1
        guard (i >= 0 && i < Seq.length slots)
        guard (all isNothing (toList (Seq.drop i slots)))
        pure (Finals (Seq.update i (Just tok) slots))
    _ -> Nothing

finalsTokens :: Finals -> [Token]
finalsTokens (Finals slots) = catMaybes (toList slots)

-- | A syllable as the nested structure of REFACTOR-AST §4: the stacks in
-- writing order, the run's span and the warnings the run recorded.
data Syllable = Syllable
    { sylStacks :: Seq Stack
    , sylSpan :: Span
    , sylDiags :: Diagnostics
    }
    deriving (Show, Eq)

-- | One vertical unit of writing. The fields are the canonical order of the
-- signs inside the stack: superfix, root, subfixes, caret, vowel, finals.
--
-- @stRoot@ is the stack's base letter whatever the role: a prefix, suffix or
-- postfix stack keeps its letter there too, and only the role says how the
-- stack relates to the root. A stack with no letter is legal and is the one
-- opened by a vowel ('VowelImplicit' or a written sign with no root).
data Stack = Stack
    { stRole :: Role
    , stSuperfix :: Maybe Token
    , stRoot :: Maybe Token
    , stSubfixes :: Subfixes
    , stVowel :: Maybe VowelSlot
    , stCaret :: Maybe Token
    , stFinals :: Finals
    }
    deriving (Show, Eq)

-- | The tokens of a stack in the canonical order of its fields.
stackTokens :: Stack -> Seq Token
stackTokens Stack{..} =
    maybe mempty Seq.singleton stSuperfix
        <> maybe mempty Seq.singleton stRoot
        <> subfixTokens stSubfixes
        <> maybe mempty Seq.singleton stCaret
        <> vowelTokens stVowel
        <> Seq.fromList (finalsTokens stFinals)
    where
        subfixTokens NoSubfix = mempty
        subfixTokens (OneSubfix t) = Seq.singleton t
        subfixTokens (TwoSubfix a b) = Seq.fromList [a, b]
        subfixTokens (ThreeSubfix a b c) = Seq.fromList [a, b, c]
        vowelTokens Nothing = mempty
        vowelTokens (Just VowelImplicit) = mempty
        vowelTokens (Just (VowelWritten t rest)) = t Seq.<| Seq.fromList rest

-- | The bridge of REFACTOR-AST step 1: group today's marked run into stacks.
--
-- The rule is the one the plan states: 'Prefix', 'Suffix' and 'Postfix' open a
-- stack of their own, 'Superfix' opens the stack whose 'Root' follows it, and
-- 'Subfix', 'Vowel', 'ImplicitVowel' and 'Final' join the stack already open.
-- A subjoined token marked 'Root' (the Tibetan half of 4.18) joins the open
-- root too: a subjoined sign cannot open a stack, so ཧྥ stays one stack.
-- Unmarked tokens (the separators the grammar ate and the run's boundary) and
-- 'Eaten' marks are not part of any stack (REFACTOR-AST §8 B2, Q6).
--
-- One limit of the bridge, recorded in REFACTOR-AST and to be fixed after the
-- main refactor: an 'ImplicitVowel' beside a written vowel is dropped (the
-- @a@ of @bo+ra@, which prints nothing and is input syntax, not a sign).
--
-- shortcut: a fourth subjoined letter (none in the corpora; the type stops at
-- 'ThreeSubfix') is dropped too - add a form when such input surfaces.
stacksOf :: Seq (Maybe Position, Token) -> Seq Stack
stacksOf = Seq.fromList . reverse . foldl' step [] . toList
    where
        step stacks (Nothing, _) = stacks
        step stacks (Just Eaten, _) = stacks
        step stacks (Just ImplicitVowel, _) = onTop stacks $ \st ->
            st{stVowel = case stVowel st of Nothing -> Just VowelImplicit; v -> v}
        step stacks (Just pos, tok) = case pos of
            Prefix -> new RolePrefix tok : stacks
            Suffix -> new RoleSuffix tok : stacks
            Postfix -> new RolePostfix tok : stacks
            Superfix -> (emptyStack RoleRoot){stSuperfix = Just tok} : stacks
            Root -> case stacks of
                (top : rest)
                    | Just _ <- stRoot top
                    , TcSubConsonant _ <- tokenCanonical tok ->
                        top{stSubfixes = pushSubfix tok (stSubfixes top)} : rest
                    | Just _ <- stSuperfix top
                    , Nothing <- stRoot top ->
                        top{stRoot = Just tok} : rest
                _ -> new RoleRoot tok : stacks
            Subfix -> onTop stacks $ \st -> st{stSubfixes = pushSubfix tok (stSubfixes st)}
            Vowel -> onTop stacks $ \st -> st{stVowel = Just (pushVowel tok (stVowel st))}
            Final
                | isCaret tok -> onTop stacks $ \st -> st{stCaret = stCaret st <|> Just tok}
                | otherwise -> onTop stacks $ \st ->
                    st{stFinals = maybe (stFinals st) id (snocFinal tok (stFinals st))}
        new role tok = (emptyStack role){stRoot = Just tok}
        emptyStack role = Stack role Nothing Nothing NoSubfix Nothing Nothing emptyFinals
        onTop [] f = [f (emptyStack RoleRoot)]
        onTop (top : rest) f = f top : rest
        pushSubfix tok NoSubfix = OneSubfix tok
        pushSubfix tok (OneSubfix a) = TwoSubfix a tok
        pushSubfix tok (TwoSubfix a b) = ThreeSubfix a b tok
        pushSubfix _ three = three
        pushVowel tok Nothing = VowelWritten tok []
        pushVowel tok (Just (VowelWritten t rest)) = VowelWritten t (rest <> [tok])
        pushVowel tok (Just VowelImplicit) = VowelWritten tok []
        isCaret t = tokenCanonical t == TcFinal FMCaret

-- | The tokens of the stacks, each in its canonical place: the inverse of
-- 'stacksOf' on the tokens it keeps. Implicit vowels, dropped subfixes, eaten
-- marks and separators have no token here (see 'stacksOf').
flatten :: Seq Stack -> Seq Token
flatten = foldMap stackTokens
