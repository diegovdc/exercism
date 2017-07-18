module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples ints  limit = 
	let  
		mod0 a b = (mod a b ) == 0
		modMap = map . mod0
		nums limit ints = filter (\n -> or (modMap n ints) ) [0..limit]
	in 
		sum $ nums (limit -1) ints