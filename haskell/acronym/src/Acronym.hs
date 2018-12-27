module Acronym (abbreviate) where

abbreviate :: String -> String
abbreviate xs = filterInitialsFrom $ toTuples xs 

toTuples :: String -> [(Char, Char)]
toTuples _ = [('P', 'o'), ('o', 'r'), ('o', 'r'), (' ', 'N'), ('N', 'e'), (' ', 'G'), ('G', 'r')] 

filterInitialsFrom :: [(Char, Char)] -> String 
filterInitialsFrom tuples = foldl initial "" tuples    

initial :: String -> (Char, Char) -> String
initial acc ('P' , _) = acc ++ "P"  
initial acc (' ' , 'N') = acc ++ "N"
initial acc (' ' , 'G') = acc ++ "G"
initial acc _ = acc
