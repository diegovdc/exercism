module School (School, add, empty, grade, sorted) where

import Data.Map(Map)
import qualified Data.Map as M
import Data.List(sort)

type Grade = Int
type Name = String
type School = Map Grade [Name]

add :: Grade -> Name -> School -> School
add gradeNum student  = M.insertWith (++) gradeNum [student] 

empty :: School
empty = M.empty 

grade :: Grade -> School -> [Name]
grade gradeNum school = M.findWithDefault [] gradeNum (sortSchool school)

sorted :: School -> [(Grade, [Name])]
sorted  = M.toList . sortSchool

sortSchool :: School -> School
sortSchool = M.map sort
