module Test.Tokenizer.Golden (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
    testGroup
        "golden"
        [ goldenVsString
            "wylie token stream"
            "test/golden/wylie_tokens.golden"
            (pure $ renderTokensBS $ tokenizeWylie "gzhon nu'i dpe cha // +")
        , goldenVsString
            "unicode token stream"
            "test/golden/unicode_tokens.golden"
            (pure $ renderTokensBS $ tokenizeUnicode "གཞོན་ནུའི་དཔེ་ཆ།། ་x")
        , goldenVsString
            "wylie diagnostics token stream"
            "test/golden/wylie_diagnostics_tokens.golden"
            (pure $ renderTokensBS $ tokenizeWylie "gzhon // ~+`]-. x")
        , goldenVsString
            "unicode edge token stream"
            "test/golden/unicode_edge_tokens.golden"
            (pure $ renderTokensBS $ tokenizeUnicode "ཀིི ཀxི ྆།")
        ]

renderTokensBS :: [Token] -> ByteString
renderTokensBS = BL.fromStrict . TE.encodeUtf8 . T.unlines . fmap renderToken

renderToken :: Token -> Text
renderToken tok =
    T.pack (show (tokenSource tok))
        <> "|"
        <> T.pack (show (tokenKind tok))
        <> "|"
        <> T.pack (show (tokenCanonical tok))
        <> "|raw="
        <> tokenRaw tok
        <> "|span="
        <> spanText (tokenSpan tok)
        <> "|issues="
        <> issuesText (tokenIssues tok)

spanText :: Span -> Text
spanText span =
    T.pack (show (offsetStart span)) <> ":" <> T.pack (show (offsetEnd span))

issuesText :: [TokenIssue] -> Text
issuesText [] = "[]"
issuesText xs = T.pack (show xs)
