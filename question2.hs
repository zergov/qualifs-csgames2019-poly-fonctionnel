module Question2 (sortlen) where

import Data.List
import Data.Char

asciiSum :: String -> Int
asciiSum = sum . map ord

-- Question 2 (1pts)
--
-- Trier une liste de chaîne de caractères en fonction de la somme ascii des
-- caractères.
sortlen :: [String] -> [String]
sortlen xs = map snd $ sortBy sortf scoredStrings
  where
    scoredStrings = map (\x -> (asciiSum x, x)) xs
    sortf (a, _) (b, _) = compare a b
