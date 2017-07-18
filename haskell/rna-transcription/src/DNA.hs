module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA xs = 
	let trans s = 
 		case s of 
	 		'G' -> Just 'C'
	 		'C' -> Just 'G'
	 		'T' -> Just 'A'
	 		'A' -> Just 'U'
	 		otherwise  -> Nothing
	in
	traverse trans xs