module Main where

import JoinListBuffer
import Editor

initialContent = map fromString
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: [JoinList (Score, Size) String]

main = runEditor editor $ foldr (+++) Empty initialContent
