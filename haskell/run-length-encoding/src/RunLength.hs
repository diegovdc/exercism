module RunLength (decode, encode) where
import Data.List(group, intercalate, splitAt, findIndex, break)
import Data.Char(isLetter, isSpace)
import Data.Maybe(fromMaybe)

encode :: String -> String
encode s = 
	let counter = map (\ch -> 
		if length ch <= 1 
		then ch 
		else show (length ch) ++ head(ch):[])
	in  (intercalate "" . counter . group) s

decode :: String -> String
decode s = decode' ("", s)

decode' :: (String, String) -> String
decode' tpl=
	let sd = split' (snd tpl)
	in case tpl of 
		(xs, "") -> xs
		(xs, _) -> decode' ( xs++repl(fst sd), snd sd)

index :: String -> Int
index = fromMaybe 0 . findIndex (\x -> isLetter x || isSpace x)

split' :: String -> (String,  String)
split' s' 
	| index s' == 0 = ("1"++ head s' :[], tail  s') 
	| otherwise = splitAt (index s' + 1) s'

splitNumChar :: String -> (String,  String)
splitNumChar s =  splitAt (index s) s

intFst :: (String,  String) -> Int
intFst tpl = read  (fst tpl) :: Int

mulChar :: (String,  String) -> [String]
mulChar tpl = replicate (intFst tpl)  (snd tpl)

repl :: String -> String
repl = intercalate "" . mulChar . splitNumChar
