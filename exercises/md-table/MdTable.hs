{-# OPTIONS_GHC -Wall #-}

module MdTable where

import Data.List (intercalate)

getMaxColumns :: [[String]] -> Int
getMaxColumns [] = 0
getMaxColumns xs = maximum [length x | x <- xs]

getMaxColumnLength :: [[String]] -> Int
getMaxColumnLength [] = 0
getMaxColumnLength xs = maximum [length x | x <- concat xs]

addEmptyStrings :: [String] -> Int -> [String]
addEmptyStrings xs 0 		= xs
addEmptyStrings [] n 		= "" : (addEmptyStrings [] (n-1))
addEmptyStrings (x:xs) n 	= x : (addEmptyStrings xs (n))

equalizeColumnCount :: [[String]] -> [[String]]
equalizeColumnCount xs = [addEmptyStrings x (getMaxColumns xs - length x) | x <- xs]

equalizeColumnSize :: [[String]] -> [[String]]
equalizeColumnSize [] 		= []
equalizeColumnSize (x:xs) 	= [pad y | y <- x] : (equalizeColumnSize xs)
	where pad z	= padWith ' ' (getMaxColumnLength (x:xs)) z
			

toLine :: [String] -> String
toLine [] = "|"
toLine (x:xs) = "| " ++ x ++ " " ++ toLine xs


padWith :: (Eq a) => a -> Int -> [a] -> [a]
padWith _ 0 x 		| x == [] 	= [] 
					| otherwise = x
padWith pad n [] 				= pad : padWith pad (n-1) []
padWith pad n (x:xs) 			= x : padWith pad (n-1) xs


layoutTable :: [[String]] -> String
layoutTable xs = intercalate "\n" [toLine x | x <- ys]
	where ys = equalizeColumnSize (equalizeColumnCount xs)

table2 :: [[String]]
table2 = [
    [ "Item", "Price" ],
    [ "iPhone", "$1", "Not in stock" ],
    [ "iPad",   "$599" ],
    [ "CAUTION: It's a scam!"]
  ]
