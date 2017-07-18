module Raindrops (convert) where

data Dropplet = Dropplet Int Sound
type Factor = Int
type Sound = String

convert :: Int -> String
convert = 	fromDropplet . 
			convertFac 7 "Plong" . 
			convertFac 5 "Plang" . 
			convertFac 3 "Pling" .
			Raindrops.pure 
		where
			convertFac :: Factor -> Sound -> Dropplet -> Dropplet
			convertFac fac sound (Dropplet n s)
				| rem n fac == 0 = Dropplet n (s++ sound)
				| otherwise = Dropplet n s

pure :: Int -> Dropplet
pure n = Dropplet n ""

fromDropplet :: Dropplet -> String
fromDropplet (Dropplet n "") = show n
fromDropplet (Dropplet _ str) = str