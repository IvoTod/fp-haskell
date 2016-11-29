module Permutations where

import Data.List hiding (permutations)

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations (x:xs) = [ y:ys | y <- (x:xs), ys <- permutations xs]
