module Grains (square, total) where

square :: Integer -> Maybe Integer
square n 
	| n < 1 || n > 64= Nothing
	| otherwise =  Just (count 0 [0] !! (64 - fromIntegral n))

total :: Integer
total = sum (count 0 [0])

count :: Integer ->  [Integer] -> [Integer]
count i acc 
	| i >= 4 = acc
	| otherwise = count  (i+1) (2^i : acc)