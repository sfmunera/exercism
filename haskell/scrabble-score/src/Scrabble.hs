module Scrabble (scoreLetter, scoreWord) where

import Data.List
import Data.Char

charInString :: Char -> String -> Bool
charInString c s = (length $ filter (\x -> x == toUpper c) s) > 0

scoreLetter :: Char -> Integer
scoreLetter letter
            | (charInString letter "AEIOULNRST") = 1
            | (charInString letter "DG") = 2
            | (charInString letter "BCMP") = 3
            | (charInString letter "FHVWY") = 4
            | (charInString letter "K") = 5
            | (charInString letter "JX") = 8
            | (charInString letter "QZ") = 10            
            | otherwise = 0

scoreWord :: String -> Integer
scoreWord word = foldl (+) 0 $ map scoreLetter word
