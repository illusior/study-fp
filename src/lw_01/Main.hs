module Main where

import Text.Printf (PrintfArg, printf)

main :: IO ()
main = do
  putStrLn "Doing task #1:\n"
  task01
  putStrLn "----------------------\n"

  putStrLn "Doing task #2:\n"
  task02
  putStrLn "----------------------\n"

  putStrLn "Doing task #3:\n"
  task03
  putStrLn "----------------------\n"

  putStrLn "Doing task #4:\n"
  task04
  putStrLn "----------------------\n"

-- putStrLn "Doing task #5:\n"
-- task05
-- putStrLn "----------------------\n"

task01 :: IO ()
task01 = do
  -- Task:
  -- Используя функции fst (возвращает первый элемент кортежа) и
  -- snd (возвращает второй элемент кортежа из 2 элементов) из стандартного модуля Prelude,
  -- чтобы "достать" значение типа Char из кортежа (( 1, 'a'), "abc")

  let tuple = ((1 :: Integer, 'a'), "abc")

  putStr "Got tuple:"
  print tuple
  let result = snd (fst tuple)

  printf "Result: %c" result

task02 :: IO ()
task02 = do
  -- Task:
  -- Используя функции head и tail получить элемент b из следующих списков
  -- 1) ['a', 'b', 'c']
  -- 2) [['a', 'b'], ['c','d']]
  -- 3) [['a', 'c', 'd'], ['a','b']]
  -- 4) [['a','d'], ['b', 'c']]

  let input01 = ['a', 'b', 'c']
  let result01 = head (tail input01)

  printTaskResult input01 result01 (1 :: Integer)

  let input02 = [['a', 'b'], ['c', 'd']]
  let result02 = head (tail (head input02))

  printTaskResult input02 result02 (2 :: Integer)

  let input03 = [['a', 'c', 'd'], ['a', 'b']]
  let result03 = head (tail (head (tail input03)))

  printTaskResult input03 result03 (3 :: Integer)

  let input04 = [['a', 'd'], ['b', 'c']]
  let result04 = head (head (tail input04))

  printTaskResult input04 result04 (4 :: Integer)

printTaskResult :: (PrintfArg t1, PrintfArg t2, Show a) => a -> t1 -> t2 -> IO ()
printTaskResult inputList result taskNumber = do
  printf "%d):\n" taskNumber
  putStr "\tGot list: "
  print inputList
  printf "\tresult: %c\n\n" result

oddNumbers :: Int -> [Int]
oddNumbers n = [x | x <- [1, 2 .. 2 * n], odd x]

oddNaturals :: Int -> Int -> [Int]
oddNaturals start end = [start, start + 2 .. end]

oddList :: Int -> [Int]
oddList 0 = []
oddList n = oddList (n - 1) ++ [2 * n - 1]

task03 :: IO ()
task03 = do
  -- Task:
  -- Выдать список нечётных натуральных чисел. Количество элементов списка = 20. (не менее 3 способов)

  let oddCount :: Int = 20
  printf "Getting first %d odd numbers in 3 different ways:\n\n" oddCount

  putStrLn "Trying `oddList` fn:"

  let listFromOddList = oddList oddCount

  print listFromOddList
  putStrLn "====================================="
  putStrLn "Trying `oddNumbers` fn:"

  let listFromOddNumbers = oddNumbers oddCount

  print listFromOddNumbers
  putStrLn "====================================="

  putStrLn "Trying `oddNaturals` fn:"

  let listFromOddNaturals = oddNaturals 1 (2 * oddCount)

  print listFromOddNaturals
  putStrLn "====================================="

fermatTriangles :: Int -> [Int]
fermatTriangles n = [x * (x + 1) `div` 2 | x <- [1 .. n]]

task04 :: IO ()
task04 = do
  -- Task:
  -- Выдать список треугольных чисел Ферма. N = 50.

  let numberCount = 50

  printf "Getting first %d numbers of Fermat's triangle sequence\n\n" numberCount

  let result = fermatTriangles numberCount

  print result

fermatPyramids :: Int -> [Int]
fermatPyramids n = [x * (x + 1) * (x + 2) `div` 6 | x <- [1 .. n]]

task05 :: IO ()
task05 = do
  -- Task:
  -- Выдать список пирамидальных чисел Ферма. N = 50

  let numberCount = 50

  printf "Getting first %d numbers of Fermat's pyramids sequence\n\n" numberCount

  let result = fermatPyramids numberCount

  print result
