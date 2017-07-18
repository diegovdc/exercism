module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard  = keep  .  (not . )

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs)
	| p x = x : keep p xs
	| otherwise =  keep p xs
