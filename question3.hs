module Question3 (countpos) where

-- Question 3 (2pts)
--
-- Soit un nombre entier positif N. Donner le nombre de possibilité d'écrire N
-- sous la forme
--
--      N = A*i + B*j + C*k,
--
-- où i, j et k sont des entiers positifs (zéro inclut) et A=5, B=15 et C=20.
constA =  5
constB = 15
constC = 20
countpos :: Int -> Int
countpos n = length candidates
  where
    candidates = [(i, j, k) | i <- [0..n], j <- [0..n], k <- [0..n], n == (constA*i + constB*j + constC*k)]
