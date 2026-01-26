module Convert.Unicode where

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import Data.Word (Word8)

-- top letters
consonants :: HashMap Char Text
consonants =
    HM.fromList
        [ ('\x0f40', "k")
        , ('\x0f41', "kh")
        , ('\x0f42', "g")
        , ('\x0f43', "g+h")
        , ('\x0f44', "ng")
        , ('\x0f45', "c")
        , ('\x0f46', "ch")
        , ('\x0f47', "j")
        , ('\x0f49', "ny")
        , ('\x0f4a', "T")
        , ('\x0f4b', "Th")
        , ('\x0f4c', "D")
        , ('\x0f4d', "D+h")
        , ('\x0f4e', "N")
        , ('\x0f4f', "t")
        , ('\x0f50', "th")
        , ('\x0f51', "d")
        , ('\x0f52', "d+h")
        , ('\x0f53', "n")
        , ('\x0f54', "p")
        , ('\x0f55', "ph")
        , ('\x0f56', "b")
        , ('\x0f57', "b+h")
        , ('\x0f58', "m")
        , ('\x0f59', "ts")
        , ('\x0f5a', "tsh")
        , ('\x0f5b', "dz")
        , ('\x0f5c', "dz+h")
        , ('\x0f5d', "w")
        , ('\x0f5e', "zh")
        , ('\x0f5f', "z")
        , ('\x0f60', "'")
        , ('\x0f61', "y")
        , ('\x0f62', "r")
        , ('\x0f63', "l")
        , ('\x0f64', "sh")
        , ('\x0f65', "Sh")
        , ('\x0f66', "s")
        , ('\x0f67', "h")
        , ('\x0f68', "a")
        , ('\x0f69', "k+Sh")
        , ('\x0f6a', "R")
        ]

-- subjoined letters
subConsonants :: HashMap Char Text
subConsonants =
    HM.fromList
        [ ('\x0f90', "k")
        , ('\x0f91', "kh")
        , ('\x0f92', "g")
        , ('\x0f93', "g+h")
        , ('\x0f94', "ng")
        , ('\x0f95', "c")
        , ('\x0f96', "ch")
        , ('\x0f97', "j")
        , ('\x0f99', "ny")
        , ('\x0f9a', "T")
        , ('\x0f9b', "Th")
        , ('\x0f9c', "D")
        , ('\x0f9d', "D+h")
        , ('\x0f9e', "N")
        , ('\x0f9f', "t")
        , ('\x0fa0', "th")
        , ('\x0fa1', "d")
        , ('\x0fa2', "d+h")
        , ('\x0fa3', "n")
        , ('\x0fa4', "p")
        , ('\x0fa5', "ph")
        , ('\x0fa6', "b")
        , ('\x0fa7', "b+h")
        , ('\x0fa8', "m")
        , ('\x0fa9', "ts")
        , ('\x0faa', "tsh")
        , ('\x0fab', "dz")
        , ('\x0fac', "dz+h")
        , ('\x0fad', "w")
        , ('\x0fae', "zh")
        , ('\x0faf', "z")
        , ('\x0fb0', "'")
        , ('\x0fb1', "y")
        , ('\x0fb2', "r")
        , ('\x0fb3', "l")
        , ('\x0fb4', "sh")
        , ('\x0fb5', "Sh")
        , ('\x0fb6', "s")
        , ('\x0fb7', "h")
        , ('\x0fb8', "a")
        , ('\x0fb9', "k+Sh")
        , ('\x0fba', "W")
        , ('\x0fbb', "Y")
        , ('\x0fbc', "R")
        ]

-- vowel signs:
-- a-chen is not here because that's a top character, not a vowel sign.
-- pre-composed "I" and "U" are dealt here; other pre-composed Sanskrit vowels are
-- more
-- easily handled by a global replace in toWylie(), b/c they turn into subjoined
-- "r"/"l".
vowel :: HashMap Char Text
vowel =
    HM.fromList
        [ ('\x0f71', "A")
        , ('\x0f72', "i")
        , ('\x0f73', "I")
        , ('\x0f74', "u")
        , ('\x0f75', "U")
        , ('\x0f7a', "e")
        , ('\x0f7b', "ai")
        , ('\x0f7c', "o")
        , ('\x0f7d', "au")
        , ('\x0f80', "-i")
        ]

-- long (Sanskrit) vowels
vowelLong :: HashMap Text Text
vowelLong =
    HM.fromList
        [ ("i", "I")
        , ("u", "U")
        , ("-i", "-I")
        , -- this is not in the original Wylie spec but this is
          -- encountered in Chinese names
          ("e", "E")
        , ("o", "O")
        ]

-- final symbols => wylie
final :: HashMap Char Text
final =
    HM.fromList
        [ ('\x0f7e', "M")
        , ('\x0f82', "~M`")
        , ('\x0f83', "~M")
        , ('\x0f37', "X")
        , ('\x0f35', "~X")
        , ('\x0f39', "^")
        , ('\x0f7f', "H")
        , ('\x0f84', "?")
        , ('\x0f85', "&")
        ]

-- final symbols by class
finalClass :: HashMap Char Text
finalClass =
    HM.fromList
        [ ('\x0f7e', "M")
        , ('\x0f82', "M")
        , ('\x0f83', "M")
        , ('\x0f37', "X")
        , ('\x0f35', "X")
        , ('\x0f39', "^")
        , ('\x0f7f', "H")
        , ('\x0f84', "?")
        , ('\x0f85', "&")
        ]

-- special characters introduced by ^
tibetanCaret :: HashMap Text Text
tibetanCaret = HM.fromList [("ph", "f"), ("b", "v")]

-- other stand-alone characters
other :: HashMap Char Text
other =
    HM.fromList
        [ (' ', "_")
        , ('\x0f04', "@")
        , ('\x0f05', "#")
        , ('\x0f06', "$")
        , ('\x0f07', "%")
        , ('\x0f08', "!")
        , ('\x0f0b', " ")
        , ('\x0f0c', "*")
        , ('\x0f0d', "/")
        , ('\x0f0e', "//")
        , ('\x0f0f', ";")
        , ('\x0f11', "|")
        , ('\x0f14', ":")
        , ('\x0f20', "0")
        , ('\x0f21', "1")
        , ('\x0f22', "2")
        , ('\x0f23', "3")
        , ('\x0f24', "4")
        , ('\x0f25', "5")
        , ('\x0f26', "6")
        , ('\x0f27', "7")
        , ('\x0f28', "8")
        , ('\x0f29', "9")
        , ('\x0f34', "=")
        , ('\x0f3a', "<")
        , ('\x0f3b', ">")
        , ('\x0f3c', "(")
        , ('\x0f3d', ")")
        ]

-- all these stacked consonant combinations don't need "+"s in them
stack :: HashSet Text
stack =
    HS.fromList
        [ "b+l"
        , "b+r"
        , "b+y"
        , "c+w"
        , "d+r"
        , "d+r+w"
        , "d+w"
        , "dz+r"
        , "g+l"
        , "g+r"
        , "g+r+w"
        , "g+w"
        , "g+y"
        , "h+r"
        , "h+w"
        , "k+l"
        , "k+r"
        , "k+w"
        , "k+y"
        , "kh+r"
        , "kh+w"
        , "kh+y"
        , "l+b"
        , "l+c"
        , "l+d"
        , "l+g"
        , "l+h"
        , "l+j"
        , "l+k"
        , "l+ng"
        , "l+p"
        , "l+t"
        , "l+w"
        , "m+r"
        , "m+y"
        , "n+r"
        , "ny+w"
        , "p+r"
        , "p+y"
        , "ph+r"
        , "ph+y"
        , "ph+y+w"
        , "r+b"
        , "r+d"
        , "r+dz"
        , "r+g"
        , "r+g+w"
        , "r+g+y"
        , "r+j"
        , "r+k"
        , "r+k+y"
        , "r+l"
        , "r+m"
        , "r+m+y"
        , "r+n"
        , "r+ng"
        , "r+ny"
        , "r+t"
        , "r+ts"
        , "r+ts+w"
        , "r+w"
        , "s+b"
        , "s+b+r"
        , "s+b+y"
        , "s+d"
        , "s+g"
        , "s+g+r"
        , "s+g+y"
        , "s+k"
        , "s+k+r"
        , "s+k+y"
        , "s+l"
        , "s+m"
        , "s+m+r"
        , "s+m+y"
        , "s+n"
        , "s+n+r"
        , "s+ng"
        , "s+ny"
        , "s+p"
        , "s+p+r"
        , "s+p+y"
        , "s+r"
        , "s+t"
        , "s+ts"
        , "s+w"
        , "sh+r"
        , "sh+w"
        , "t+r"
        , "t+w"
        , "th+r"
        , "ts+w"
        , "tsh+w"
        , "z+l"
        , "z+w"
        , "zh+w"
        ]

-- a map used to split the input string into tokens for toUnicode().
-- all letters which start tokens longer than one letter are mapped to the max
-- length of tokens starting with that letter.

tokensStart :: HashMap Char Word8
tokensStart =
    HM.fromList
        [ ('S', 2)
        , ('/', 2)
        , ('d', 4)
        , ('g', 3)
        , ('b', 3)
        , ('D', 3)
        , ('z', 2)
        , ('~', 3)
        , ('-', 4)
        , ('T', 2)
        , ('a', 2)
        , ('k', 2)
        , ('t', 3)
        , ('s', 2)
        , ('c', 2)
        , ('n', 2)
        , ('p', 2)
        , ('\r', 2)
        ]

-- also for tokenization - a set of tokens longer than one letter
tokens :: HashSet Text
tokens =
    HS.fromList
        [ "k+Sh"
        , "b+l"
        , "-d+h"
        , "dz+h"
        , "-dh"
        , "-sh"
        , "-th"
        , "D+h"
        , "b+h"
        , "d+h"
        , "dzh"
        , "g+h"
        , "tsh"
        , "~M`"
        , "-I"
        , "-d"
        , "-i"
        , "-n"
        , "-t"
        , "//"
        , "Dh"
        , "Sh"
        , "Th"
        , "ai"
        , "au"
        , "bh"
        , "ch"
        , "dh"
        , "dz"
        , "gh"
        , "kh"
        , "ng"
        , "ny"
        , "ph"
        , "sh"
        , "th"
        , "ts"
        , "zh"
        , "~M"
        , "~X"
        , "\r\n"
        ]

