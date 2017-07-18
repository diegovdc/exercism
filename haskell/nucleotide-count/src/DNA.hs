module DNA (nucleotideCounts) where

import Data.Map(Map, fromList, insertWithKey)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts  s
	| s == "" = Right initial
	| not . and $ map (\x ->  x `elem` "AGTC") s = Left "error"
	| otherwise =  Right $ inc' initial s

initial :: Map Char Int
initial =  fromList [('A', 0), ('G', 0), ('T', 0), ('C', 0)]

inc' :: Map Char Int -> [Char] -> Map Char Int
inc' m [] = m
inc' m (x:xs) = inc' (inc m x) xs

inc :: Map Char Int -> Char -> Map Char Int
inc m key = 
	insertWithKey add key 0 m
	where
		add k nv ov  = ov+1
