{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  putStrLn "===================== TASK 01 ====================="
  task01
  putStrLn "===================== TASK 01 ====================="
  putStrLn ""

  putStrLn "===================== TASK 02 ====================="
  task02
  putStrLn "===================== TASK 02 ====================="
  putStrLn ""

  putStrLn "===================== TASK 03 ====================="
  task03
  putStrLn "===================== TASK 03 ====================="
  putStrLn ""

  putStrLn "===================== TASK 04 ====================="
  task04
  putStrLn "===================== TASK 04 ====================="
  putStrLn ""

  putStrLn "===================== TASK 05 ====================="
  task05
  putStrLn "===================== TASK 05 ====================="
  putStrLn ""

-- ===================== TASK 01 =====================

-- Определите функцию listnums.
-- Она берет численный аргумент n и возвращает список всех чисел от n до 1, включительно.

listnums :: Int -> [Int]
listnums n | n <= 0 = []
listnums n | n == 1 = [1]
listnums n = [n] ++ listnums (n - 1)

task01 :: IO ()
task01 = do
  let n = 22
  putStr "N is equal to "
  print n
  putStrLn ""

  let nTo1 = listnums n
  print nTo1
  putStrLn ""

-- ===================== TASK 01 =====================

-- ===================== TASK 02 =====================

-- Определите функцию secondlastlist.
-- Эта функция берет список списков и возвращает последние элементы каждого, объединенные в список.

secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (xs:xss) | length xs == 0 = secondlastlist xss
                        | otherwise = last xs : secondlastlist xss


task02 :: IO ()
task02 = do
  let list :: [[Int]] = [[], [3], [5, 6], [7, 8, 9], []]
  putStr "Here a list: "
  print list
  putStrLn ""

  let secondLastList = secondlastlist list

  putStr "Here is a result of using secondlastlist: "
  print secondLastList
  putStrLn ""

-- ===================== TASK 02 =====================

-- ===================== TASK 03 =====================

-- Определите функцию myunion, которая находит объединение двух списков.
-- Объединением двух списков будет список содержащий элементы, которые есть по крайней мере в одном из списков.

myunion :: (Eq a) => [a] -> [a] -> [a]
myunion [] _ = []
myunion _ [] = []
myunion xs (y:ys) = foldl
                      (\acc x -> if x == y then x : acc else acc)
                      []
                      xs
                    ++ myunion xs ys

task03 :: IO ()
task03 = do
  let list1 :: [Int] = [3, 6, 1, 1, 0]
  let list2 :: [Int] = [1, 2, 3, 4, 5, 6]
  putStr "Here is a pair of lists: "
  print list1
  print list2
  putStrLn ""

  putStrLn "Here is a result of using myunion fn: "
  let result = myunion list1 list2
  print result
  putStrLn ""

-- ===================== TASK 03 =====================

-- ===================== TASK 04 =====================

-- Определите функцию mysubst.
-- Получив два списка, она возвращает их разность.
-- Разность двух списков называется список, состоящий из элементов  первого списка, которые не принадлежат второму списку.

mysubst :: (Eq a) => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst _ [] = []
mysubst xs ys = foldr (\x acc -> if any (==x) ys then acc else x : acc)
                []
                xs

task04 :: IO ()
task04 = do
  let list1 :: [Int] = [1, 2, 3, 4, 5, 6]
  let list2 :: [Int] = [3, 6, 1, 0]
  putStr "Here is a pair of lists: "
  print list1
  print list2
  putStrLn ""

  putStrLn "Here is a result of using mysubst fn: "
  let result = mysubst list1 list2
  print result
  putStrLn ""

-- ===================== TASK 04 =====================

-- ===================== TASK 05 =====================

-- Напишите функцию, берущую список списков и возвращающую
-- список из N-х элементов подсписков с помощью функций map и (!!)

takeNthFromSublists :: Int -> [[a]] -> [a]
takeNthFromSublists n xs = catMaybes (
    map (safeGetNth n) xs
  )
  where
    safeGetNth n xs | length xs >= n && n >= 0 = Just (xs !! (n - 1))
                    | otherwise = Nothing


task05 :: IO ()
task05 = do
  let list :: [[Int]] = [[], [3], [1, 5, 6], [7, 8, 9]]
  putStr "Here a list: "
  print list
  putStrLn ""

  let result = takeNthFromSublists 1 list

  putStr "Here is a result of using takeNthFromSublists: "
  print result
  putStrLn ""

-- ===================== TASK 05 =====================
