module Question4 (countcase) where
import qualified Data.Set as Set

-- Question 4 (3pts)
--
-- Soit les caractères '^', '>', 'v' et '<' définissant respectivement un
-- mouvement de une unité vers le haut, la droite, le bas et la gauche. Soit
-- une suite de mouvement, donné le nombre de case unique visitée.
--
-- La suite "<><><><><><><>" visite 2 cases différentes.
-- La suite "<^>v" visite 4 cases différentes.
-- La suite "<v<v<v<v<" visite 10 cases différentes,

vdirection :: Char -> (Int, Int)
vdirection '^' = (0, 1)
vdirection 'v' = (0, -1)
vdirection '>' = (1, 0)
vdirection '<' = (-1, 0)

countcase :: String -> Int
countcase xs = length $ Set.fromList visited
  where
    visited = foldl (\acc d -> (vdirection d) : acc) [(0, 0)] xs
