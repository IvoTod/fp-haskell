module Permutations where

import Data.List hiding (permutations)

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [ y:ys | y <- xs, ys <- permutations (delete y xs)]
