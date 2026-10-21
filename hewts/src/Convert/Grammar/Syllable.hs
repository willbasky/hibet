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
-- 'Role', 'Subfixes', 'VowelSlot' and 'Finals'. Since step 4d the grammar
-- places every sign into those stacks as it parses ('placeToken', reached from
-- 'Convert.Grammar.Parser.mark'), and the sentence takes the stacks from the
-- parser state. The grammar still returns the flat marks of its run as well;
-- the windows and the structural tests read them, and step 4e removes them
-- together with 'Position'.
module Convert.Grammar.Syllable
    ( Position (..)
    , TibetanSyllable
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
    , placeToken
    , flatten
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Convert.Diagnostic (Diagnostics)
import Convert.Token
    ( FinalMark (..)
    , Span
    , Token
    , TokenCanonical (..)
    , finalSlot
    , tokenCanonical
    )
import Data.Foldable (toList)
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
    deriving (Show, Eq, Ord)

-- | A Tibetan syllable (Def 4.10) as a flat list of marked tokens.
--
-- The marks are what a rule placed where: they serve the windows that read a
-- finished syllable ('Convert.Grammar.Constraint.Constraint22') and the
-- structural tests. The stacks themselves no longer come from this list: since
-- REFACTOR-AST step 4d the grammar places every mark into the parser state as
-- it goes ('Convert.Grammar.Parser.mark'), and this list is removed together
-- with 'Position' in step 4e. A 'Seq' rather than a list because the syllable
-- is built by appending pieces (a prefix, a root, a subfix, a vowel) and read
-- from both ends by the legality rules of wave 3.4. Be aware that this buys
-- clarity more than speed: a syllable holds one to six letters, and at that
-- size 'Seq' and a list append in the same handful of steps. The real cost in
-- this layer is elsewhere - see 'Convert.Grammar.Structure.pStructure', which
-- probes all 37 structures in lookahead, so every syllable is parsed 37 times
-- over.
type TibetanSyllable = Seq (Position, Token)

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
-- or whose chain has passed it. The grammar drops such a mark before it gets
-- here, so a refusal means the placement met a shape the grammar did not allow.
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

-- | Put one marked token into the stacks of a syllable, in the canonical order
-- of the fields. The grammar's own placement ('Convert.Grammar.Parser.mark')
-- goes through here, so the group of a mark is decided once: a prefix, suffix
-- or postfix opens a stack of its own, a superfix opens the stack whose root
-- follows it, a subjoined root joins the open root, and a subfix, vowel,
-- implicit vowel, caret or final joins the stack already open. An
-- 'ImplicitVowel' beside a written vowel is dropped: it prints nothing and is
-- input syntax, not a sign (the @a@ of @bo+ra@).
--
-- shortcut: a fourth subjoined letter (none in the corpora; the type stops at
-- 'ThreeSubfix') is dropped - add a form when such input surfaces.
placeToken :: Position -> Token -> Seq Stack -> Seq Stack
placeToken pos tok stacks = case pos of
    Prefix -> stacks Seq.|> new RolePrefix tok
    Suffix -> stacks Seq.|> new RoleSuffix tok
    Postfix -> stacks Seq.|> new RolePostfix tok
    Superfix -> stacks Seq.|> (emptyStack RoleRoot){stSuperfix = Just tok}
    Root -> case Seq.viewr stacks of
        rest Seq.:> top
            | Just _ <- stRoot top
            , TcSubConsonant _ <- tokenCanonical tok ->
                rest Seq.|> top{stSubfixes = pushSubfix tok (stSubfixes top)}
            | Just _ <- stSuperfix top
            , Nothing <- stRoot top ->
                rest Seq.|> top{stRoot = Just tok}
        _ -> stacks Seq.|> new RoleRoot tok
    Subfix -> onTop $ \st -> st{stSubfixes = pushSubfix tok (stSubfixes st)}
    Vowel -> onTop $ \st -> st{stVowel = Just (pushVowel tok (stVowel st))}
    ImplicitVowel -> onTop $ \st ->
        st{stVowel = case stVowel st of Nothing -> Just VowelImplicit; v -> v}
    Final
        | isCaret tok -> onTop $ \st -> st{stCaret = stCaret st <|> Just tok}
        | otherwise -> onTop $ \st ->
            st{stFinals = maybe (stFinals st) id (snocFinal tok (stFinals st))}
    where
        new role tok' = (emptyStack role){stRoot = Just tok'}
        emptyStack role = Stack role Nothing Nothing NoSubfix Nothing Nothing emptyFinals
        onTop f = case Seq.viewr stacks of
            Seq.EmptyR -> Seq.singleton (f (emptyStack RoleRoot))
            rest Seq.:> top -> rest Seq.|> f top
        pushSubfix tok' NoSubfix = OneSubfix tok'
        pushSubfix tok' (OneSubfix a) = TwoSubfix a tok'
        pushSubfix tok' (TwoSubfix a b) = ThreeSubfix a b tok'
        pushSubfix _ three = three
        pushVowel tok' Nothing = VowelWritten tok' []
        pushVowel tok' (Just (VowelWritten t rest)) = VowelWritten t (rest <> [tok'])
        pushVowel tok' (Just VowelImplicit) = VowelWritten tok' []
        isCaret t = tokenCanonical t == TcFinal FMCaret

-- | The tokens of the stacks, each in its canonical place. Implicit vowels,
-- dropped subfixes and separators have no token here.
flatten :: Seq Stack -> Seq Token
flatten = foldMap stackTokens
