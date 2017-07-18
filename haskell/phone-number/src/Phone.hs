module Phone (number) where

import Data.Char

number :: String -> Maybe String
number = number' . clean
	where
		clean  = filter $ not . (flip elem) "()-. "

number' :: String -> Maybe String
number' num
	| or $ map (\ch -> isAlpha ch || isPunctuation ch) num = Nothing
	| length num == 10 = Just num
	| length num == 11 && head num == '1' = Just (tail num)
	| otherwise = Nothing

