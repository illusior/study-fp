{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

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

  putStrLn "===================== TASK 06 ====================="
  task06
  putStrLn "===================== TASK 06 ====================="
  putStrLn ""

  putStrLn "===================== TASK 07 ====================="
  task07
  putStrLn "===================== TASK 07 ====================="
  putStrLn ""

-- ===================== TASK 01 =====================

-- Написать реализацию следующей функции:
-- do_my_list :: Int-> [Int]
-- параметром является число N. функция генерирует список из N элементов, начиная с указанного элемента N

task01 :: IO ()
task01 = do
  let my_list = do_my_list 25

  print my_list

do_my_list :: Int -> [Int]
do_my_list n | n <= 0 = []
do_my_list n | n == 1 = [1]
do_my_list n = drop (n - 1) [1 .. (2 * n)]

-- ===================== TASK 01 =====================

-- ===================== TASK 02 =====================

-- Написать функцию oddEven
-- oddEven(L) - функция перестановки местами соседних элементов списка L
-- (Например, список [2,5,7,9,1,8] после преобразования будет иметь вид [5,2,9,7,8,1])

task02 :: IO ()
task02 = do
  let myList :: [Int] = [1, 2, 3, 0, 9, 8, 7, 4, 5]

  print "Got list: "
  print myList
  putStrLn ""

  putStrLn "After oddEven swap:"
  print (oddEven myList)
  putStrLn ""

oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven (x : y : xs) = y : x : oddEven xs

-- ===================== TASK 02 =====================

-- ===================== TASK 03 =====================

-- Написать
-- insert (L,A,n) - функция включения в список L заданного атома А на определенную позицию n.

task03 :: IO ()
task03 = do
  let myList :: [Int] = [1, 2, 3, 0, 9, 8, 7, 4, 5]
  let toInsert :: Int = 999
  let atPos :: Int = 7

  print "Got list: "
  print myList
  putStrLn ""

  print "Got to insert number: "
  print toInsert
  putStrLn ""

  print "Got insert index: "
  print atPos
  putStrLn ""

  putStrLn "After inserting:"
  print (insert myList toInsert atPos)
  putStrLn ""

insert :: [a] -> a -> Int -> [a]
insert xs _ n | n <= 0 = xs
insert xs x n = take n xs ++ [x] ++ drop n xs

-- ===================== TASK 03 =====================

-- ===================== TASK 04 =====================

-- Напишите
-- listSum(L1,L2) - функция сложения элементов двух списков. Возвращает список, составленный из сумм элементов списков - параметров L1, L2.
-- Учесть, что переданные списки могут быть разной длины

task04 :: IO ()
task04 = do
  let myList1 :: [Int] = [1, 2, 6, 3, 0, 9, 8, 7, 4, 5]
  let myList2 :: [Int] = take (length myList1 - 1) (reverse myList1)

  print "Got list1: "
  print myList1
  putStrLn ""

  print "Got list2: "
  print myList2
  putStrLn ""

  print "Sum: "
  print (listSum myList1 myList2)
  putStrLn ""

listSum :: (Num a) => [a] -> [a] -> [a]
listSum [] [] = []
listSum [] (x : xs) = x : xs
listSum (x : xs) [] = x : xs
listSum (x : axs) (y : bxs) = (x + y) : listSum axs bxs

-- ===================== TASK 04 =====================

-- ===================== TASK 05 =====================

-- Написать функцию
-- position (L, A)- возвращает номер первого вхождения заданного атома А в список L.

task05 :: IO ()
task05 = do
  let toSearch :: Int = -10
  let myList :: [Int] = [0, 1, 2, -10, 100, 9]

  putStr "Searching position of "
  print toSearch
  putStr "In list: "
  print myList
  putStrLn ""

  let pos = position toSearch myList
  putStr "Found at "
  print pos
  putStrLn ""

position :: (Eq a) => a -> [a] -> Int
position _ [] = -1
position x (y : xs)
  | x == y = 0
  | otherwise = 1 + position x xs

-- ===================== TASK 05 =====================

-- ===================== TASK 06 =====================

-- Написать функцию
-- fn(n) = 1+2+3+...+n

task06 :: IO ()
task06 = do
  let n = 12
  putStr "Sum up to "
  print n
  putStr "is "
  print (sumUpToN n)
  putStrLn ""

sumUpToN :: Int -> Int
sumUpToN n | n <= 0 = 0
sumUpToN n = sum [1 .. n]

-- ===================== TASK 06 =====================

-- ===================== TASK 07 =====================

-- Написать функцию
-- fn(n) = (n-1)+(n-2)...+(n-n)

task07 :: IO ()
task07 = do
  let n = 12
  putStr "Sum up to "
  print n
  putStr "is "
  print (sumUpToN_1 n)
  putStrLn ""

sumUpToN_1 :: Int -> Int
sumUpToN_1 n | n <= 0 = 0
sumUpToN_1 n | n > 1000 = n * (n - 1) `div` 2
sumUpToN_1 n = sum [n - x | x <- [1 .. n]]

-- ===================== TASK 07 =====================
