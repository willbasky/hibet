{- | Public interface for the tokenizer layer.

'Tokenizer' turns source text (Unicode Tibetan or Wylie) into a token stream
('Token'), which the grammar layer then consumes. Both tokenizers produce the
same canonical 'Convert.Token.Token' representation, so downstream layers are
source-agnostic.
-}

module Convert.Tokenizer
    ( tokenizeUnicode
    , tokenizeWylie
    ) where

import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
