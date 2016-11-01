import Data.Char(toLower)

vowels = "aeiou"

isChar :: Char -> Bool
isChar char 
	| toLower char >= 'a' && toLower char <= 'z' = True
isChar _ = False

isInString :: Char -> String -> Bool
isInString char [] = False
isInString char (first:rest)
	| char == first = True
	| char /= first = isInString char rest


isVowel :: Char -> Bool
isVowel char
	| isInString (toLower char) vowels == True = True
isVowel _ = False


isConsonant :: Char -> Bool
isConsonant char
	| isVowel char == False && isChar char == True = True
	| otherwise = False


encode :: String -> String
encode [] = []
encode (first:rest)
	| isConsonant first == True = first : 'o' : first : (encode rest)
	| otherwise = first:(encode rest)


-- Bonus #1
encode' :: String -> String
encode' [] = []
encode' (first:rest)
	| isConsonant first == True = first : 'o' : toLower first : (encode' rest)
	| otherwise = first:(encode' rest)


-- Bonus #2
dropN :: Int -> String -> String
dropN number [] = []
dropN 0 string = string
dropN number (first:rest) = dropN (number-1) rest



--Assume we'll be decoding only valid words
decode :: String -> String
decode [] = []
decode (x : 'o' : y : rest)
	| x == y = x : decode (dropN 2 ('o':y:rest))
	| otherwise = x : 'o' : y : (decode (rest))
decode (first : rest) = first : decode (rest)
