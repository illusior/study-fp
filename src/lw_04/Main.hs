module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- concat flattens a list of lists into just a list of elements.
my_concat :: [[a]] -> [a]
my_concat xs = Prelude.foldr (++) [] xs

-- The transpose function transposes the rows and columns of its argument.
my_transpose               :: [[a]] -> [[a]]
my_transpose []             = []
my_transpose ([]   : xss)   = my_transpose xss
my_transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : my_transpose (xs : [ t | (_:t) <- xss])

-- intersperse takes an element and a list and then puts that element in between each pair of elements in the list
my_intersperse :: a -> [a] -> [a]
my_intersperse sep xs = my_sub_intersperse sep (init xs) ++ [last xs]

my_sub_intersperse :: a -> [a] -> [a]
my_sub_intersperse sep xs = List.foldr (\x acc -> [x] ++ [sep] ++ acc) [] xs

-- looks up some value given a key
my_lookup :: (Eq k) => k -> [(k,v)] -> Maybe v
my_lookup key [] = Nothing
my_lookup key ((k,v):xs) = if key == k
                            then Just v
                            else my_lookup key xs

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]


-- see which letters are in the first set but aren't in the second
my_difference :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
my_difference a b = Prelude.foldr f Set.empty a
  where
    f x s
      | Set.member x b = s
      | otherwise     = Set.insert x s


text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

set1 = Set.fromList text1
set2 = Set.fromList text2


main :: IO ()
main = do
  print (my_concat ["foo","bar","car"])              --"foobarcar"
  print (my_concat [[3,4,5],[2,3,4],[2,1,1]])        -- [3,4,5,2,3,4,2,1,1]

  print ( my_transpose [[1,2,3],[4,5,6],[7,8,9]] )   -- [[1,4,7],[2,5,8],[3,6,9]]
  print ( my_transpose ["hey","there","guys"] )      --["htg","ehu","yey","rs","e"]

  print (my_intersperse '.' "Zhir")
  print (my_intersperse 0 [1,2,3,4,5,6])             -- [1,0,2,0,3,0,4,0,5,0,6]

  print (my_lookup "penny" phoneBook)                -- "Just 853-2492"
  print (my_lookup "pontiy" phoneBook)               -- "Nothing"

  print (my_difference set1 set2 )                   -- ".?AIRj"
  print (my_difference set2 set1 )                   -- "!Tbcgvw"

