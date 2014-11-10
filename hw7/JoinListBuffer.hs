{-# LANGUAGE FlexibleInstances #-}

module JoinListBuffer where

import JoinList
import Buffer
import Sized
import Scrabble

-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- exercise 4
instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ j1 j2) = toString j1 ++ toString j2
    replaceLine n l b = takeJ (n-1) b +++ fromString l +++ dropJ n b
    fromString str = Single (m, 1) str
        where (Single m _) = scoreLine str

    line n = indexJ n
    numLines = getSize . size . tag
    value b = getScore s
        where (s, _) = tag b
