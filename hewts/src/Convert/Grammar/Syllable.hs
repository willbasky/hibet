-- | A parsed Tibetan syllable (REFACTOR-AST §4): the stacks of one run, in
-- writing order, and the run's tokens in the order they were read.
--
-- The vocabulary is the book's own, from "Research on Tibetan Spelling Formal
-- Language and Automata with Application" (Nyima Tashi, Science Press + Springer,
-- 2019), section 4.1, Definitions 4.1-4.8. Our 37 structures come from the same
-- book (its "Tibetan spelling grammar 4.1-4.20", section 4.2).
--
-- The role of a letter is a field of its stack, and the slot inside the stack is
-- the field itself ('Stack.stSuperfix', 'stRoot' and so on). The parser places
-- each sign through the 'put…' functions below, one per field; the flat run of
-- tokens is what the structures return and what the coverage checks read, and it
-- carries no marks (step 4e removed 'Position').
module Convert.Grammar.Syllable
    ( TibetanSyllable
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
    , putPrefix
    , putSuffix
    , putPostfix
    , putSuperfix
    , putRoot
    , putSubfix
    , putVowel
    , putImplicitVowel
    , putFinal
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

-- | The tokens of a run, in the order they were read. The stacks of the run are
-- the grammar's own state ('Convert.Grammar.Parser.takeStacks'); this flat list
-- is what the structures hand back, so the coverage checks and the spans have
-- the run without consulting the stacks.
type TibetanSyllable = Seq Token

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

-- | Put one token into its slot. One function per field of 'Stack': a prefix,
-- suffix or postfix opens a stack of its own, a superfix opens the stack whose
-- root follows it, a subjoined root joins the open root, and a subfix, vowel,
-- implicit vowel or final fills the field of the stack already open.
--
-- shortcut: a fourth subjoined letter (none in the corpora; the type stops at
-- 'ThreeSubfix') is dropped - add a form when such input surfaces.
putPrefix
    , putSuffix
    , putPostfix
    , putSuperfix
    , putRoot
    , putSubfix
    , putVowel
    , putImplicitVowel
    , putFinal ::
        Token -> Seq Stack -> Seq Stack
putPrefix tok = opens RolePrefix tok
putSuffix tok = opens RoleSuffix tok
putPostfix tok = opens RolePostfix tok
putSuperfix tok stacks = stacks Seq.|> (emptyStack RoleRoot){stSuperfix = Just tok}
putRoot tok stacks = case Seq.viewr stacks of
    rest Seq.:> top
        | Just _ <- stRoot top
        , TcSubConsonant _ <- tokenCanonical tok ->
            rest Seq.|> top{stSubfixes = pushSubfix tok (stSubfixes top)}
        | Just _ <- stSuperfix top
        , Nothing <- stRoot top ->
            rest Seq.|> top{stRoot = Just tok}
    _ -> stacks Seq.|> (emptyStack RoleRoot){stRoot = Just tok}
putSubfix tok = onTop $ \st -> st{stSubfixes = pushSubfix tok (stSubfixes st)}
putVowel tok = onTop $ \st -> st{stVowel = Just (pushVowel tok (stVowel st))}

-- | The implicit @a@ is a fact of the slot, not a sign: it prints nothing and is
-- input syntax, so a written vowel beside it wins (the @a@ of @bo+ra@).
putImplicitVowel _ = onTop $ \st ->
    st{stVowel = case stVowel st of Nothing -> Just VowelImplicit; v -> v}

putFinal tok
    | isCaret tok = onTop $ \st -> st{stCaret = stCaret st <|> Just tok}
    | otherwise = onTop $ \st ->
        st{stFinals = maybe (stFinals st) id (snocFinal tok (stFinals st))}

opens :: Role -> Token -> Seq Stack -> Seq Stack
opens role tok stacks = stacks Seq.|> (emptyStack role){stRoot = Just tok}

emptyStack :: Role -> Stack
emptyStack role = Stack role Nothing Nothing NoSubfix Nothing Nothing emptyFinals

onTop :: (Stack -> Stack) -> Seq Stack -> Seq Stack
onTop f stacks = case Seq.viewr stacks of
    Seq.EmptyR -> Seq.singleton (f (emptyStack RoleRoot))
    rest Seq.:> top -> rest Seq.|> f top

pushSubfix :: Token -> Subfixes -> Subfixes
pushSubfix tok NoSubfix = OneSubfix tok
pushSubfix tok (OneSubfix a) = TwoSubfix a tok
pushSubfix tok (TwoSubfix a b) = ThreeSubfix a b tok
pushSubfix _ three = three

pushVowel :: Token -> Maybe VowelSlot -> VowelSlot
pushVowel tok Nothing = VowelWritten tok []
pushVowel tok (Just (VowelWritten t rest)) = VowelWritten t (rest <> [tok])
pushVowel tok (Just VowelImplicit) = VowelWritten tok []

isCaret :: Token -> Bool
isCaret t = tokenCanonical t == TcFinal FMCaret

-- | The tokens of the stacks, each in its canonical place. Implicit vowels,
-- dropped subfixes and separators have no token here.
flatten :: Seq Stack -> Seq Token
flatten = foldMap stackTokens
