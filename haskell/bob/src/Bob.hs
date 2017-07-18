module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs 
	| silence  = "Fine. Be that way!"
	| shout = "Whoa, chill out!"
	| question = "Sure."
	| otherwise = "Whatever."
	where 
		silence = all isSpace xs
		shout = every isUpper (filter isLetter xs)
		question = last (dropWhileEnd isSpace xs) == '?'

every :: (a -> Bool) -> [a] -> Bool
every _ [] = False
every pred list = all pred list

