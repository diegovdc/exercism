module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

type Legacy = (Int, String)
type Shiny = (Char, Int)

transform :: Map Int String -> Map Char Int
transform = fromList . foldl (++) [] . map split  . toList 

split :: Legacy -> [Shiny]
split (int, str) = map (\ch -> ((toLower ch), int)) str