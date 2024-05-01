module Main where

import Data.Char ( isPunctuation, toUpper )
import Data.Foldable ( foldl' )
import Data.List ( find )
import Data.Set ( Set )
import GHC.IO.Handle.FD ( openFile )
import qualified Data.Set as Set

import System.Directory ( renameFile )
import System.IO ( hClose
                 , hGetContents
                 , hPutStr
                 , openTempFile
                 , IOMode(ReadMode)
                 )

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

-- ===================== TASK 01 =====================

-- Напишите функцию partitionN :: [a] -> Int ->[[a]], которая разбивает заданный список на указанное количество подсписков.
-- N-1 подсписков должны иметь одинаковую длину,
-- последний элемент списка списков может содержать меньшее количество элементов.
-- Предусмотреть контроль входных данных.

-- Например:
-- partitionN [1,2,3,4,5,6,7] 3 даст результат [[1,2,3], [4,5,6], [7]]
-- partitionN [1,2,3,4,5,6,7] 4 даст результат [[1,2], [3,4], [5,6], [7]]

task01 :: IO ()
task01 = do
  let list :: [Integer] = [1,2,3,4,5,6,7]
  putStrLn ("Имеем список: " ++ show list)

  let n1 = 3
  let n2 = 4
  let n3 = 8
  let n4 = 5
  putStrLn ("Применяем partitionN c N == " ++ show n1 ++ ": " ++ show (partitionN list n1))
  putStrLn ("Применяем partitionN c N == " ++ show n2 ++ ": " ++ show (partitionN list n2))
  putStrLn ("Применяем partitionN c N == " ++ show n3 ++ ": " ++ show (partitionN list n3))
  putStrLn ("Применяем partitionN c N == " ++ show n4 ++ ": " ++ show (partitionN list n4))

partitionN :: [a] -> Int -> [[a]]
partitionN [] _ = []
partitionN xs n | n <= 0 = error "n must be positive"
                | otherwise = filter (not . null) (splitBy (length xs `div` n + 1) xs)

splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy sublistSize xs = take sublistSize xs :
                        if length (drop sublistSize xs) >= sublistSize then
                          splitBy sublistSize (drop sublistSize xs)
                        else
                          [drop sublistSize xs]

-- ===================== TASK 01 =====================

-- ===================== TASK 02 =====================

-- Напишите функцию elemIndices :: Eq a => a -> [a] -> [Int], которая находит,
-- под какими индексами в списке встречается заданный элемент.

task02 :: IO ()
task02 = do
  let list :: [Integer] = [4, 4, 1, 2, 3, 4, 5, 6, 4, 7, 4, 4, 4]
  putStrLn ("Имеем список: " ++ show list)

  let toSearch1 :: Integer = 3
  let toSearch2 :: Integer = 4
  let toSearch3 :: Integer = 8
  putStrLn ("Применяем elemIndices c toSearch == " ++ show toSearch1 ++ ": " ++ show (elemIndices toSearch1 list))
  putStrLn ("Применяем elemIndices c toSearch == " ++ show toSearch2 ++ ": " ++ show (elemIndices toSearch2 list))
  putStrLn ("Применяем elemIndices c toSearch == " ++ show toSearch3 ++ ": " ++ show (elemIndices toSearch3 list))

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x xs = foldl (\findsAt (i, el) ->
                           if el == x
                             then findsAt ++ [i]
                             else findsAt
                         ) [] (zip [0..] xs)

-- ===================== TASK 02 =====================

-- ===================== TASK 03 =====================

-- Напишите функцию, строящую список подсписков чисел:
-- в первом подсписке будут степени единицы, во втором степени двойки, в третьем - тройки и так далее:

-- [[1,1,1,...],[2,4,8,...],[3,9,27 ,...],...]

task03 :: IO ()
task03 = do
  let count = 10
  putStrLn ("Получить список " ++ (show count) ++ " списков, где каждый список состоит из первых " ++ show count ++ " степеней числа")
  putStrLn ("Числом считать номер подсписка в списке:")
  let result = getListsOfExps count
  printIndexedList result

printIndexedList :: [[Int]] -> IO ()
printIndexedList xs = putStrLn $ "[\n" ++
  unlines (
    map (\(i, ys) -> "\t" ++ show (i + 1) ++
                     ": " ++
                     show ys ++
                     if i + 1 == length xs then "" else ","
        ) (zip [0..] xs)
  ) ++ "]"

getExp :: Int -> Int -> Int
getExp base exp_ | exp_ < 0 = 0
                        | exp_ == 0 = 1
                        | otherwise = do
                          let repeatedExpTimesBase = replicate exp_ base
                          let degree = foldl (\acc x -> acc * x) base repeatedExpTimesBase
                          degree

getExps :: Int -> Int -> [Int]
getExps base times | times <= 0 = []
                   | otherwise = do
                     let exps = foldl' (\acc x -> acc ++ [getExp base x]) [] [1..pred times]
                     base : exps

getListsOfExps :: Int -> [[Int]]
getListsOfExps times = drop 1 (foldl' (\acc x -> acc ++ [getExps x times]) [] [0..pred times + 1])


-- ===================== TASK 03 =====================

-- ===================== TASK 04 =====================

-- Напишите функцию, которая читает входной текстовый файл и выводит в выходной файл пары (слово:число),
-- где слово - есть каждое уникальное слово файла, а число - количество вхождений этого слова.
-- Пары должны быть отсортированы по убыванию чисел.

data CountedWord = CountedWord
  { word :: String,
    count :: Int
  }
  deriving (Eq)

instance Ord CountedWord where
  compare :: CountedWord -> CountedWord -> Ordering
  compare (CountedWord w1 c1) (CountedWord w2 c2) =
    let wordCompare = compare w1 w2
        countCompare = compare c2 c1
    in if countCompare == EQ then wordCompare else countCompare

instance Show CountedWord where
  show :: CountedWord -> String
  show (CountedWord word count) = "" ++ word ++ ": " ++ show count

task04 :: IO ()
task04 = do
  let fileInPath = ".\\text.in"
  let fileOutPath = ".\\result.out"

  putStrLn ("Считаем вхождения слов в файле по пути '" ++ show fileInPath ++ "'")

  printFromFileCountWordsToFile fileInPath fileOutPath

  putStrLn ("Результат работы выведен в файл по пути '" ++ show fileOutPath ++ "'")

printFromFileCountWordsToFile :: FilePath -> FilePath -> IO ()
printFromFileCountWordsToFile toAnalyzeFile resultPath = do
  handle <- openFile toAnalyzeFile ReadMode
  text <- (hGetContents handle)

  let setOfCountedWords = countWordsInText text

  let printableCountedWordSetAsList = showCountedWordSet setOfCountedWords

  tempFileName <- writeToLocalTempFile printableCountedWordSetAsList
  renameFile tempFileName resultPath

  hClose handle

printCountedWordSet :: Set CountedWord -> IO ()
printCountedWordSet countedWordSet = foldl' (\acc countedWord -> acc >> putStrLn (show countedWord)) (return ()) countedWordSet

showCountedWordSet :: Set CountedWord -> [String]
showCountedWordSet countedWordSet = foldl' (\acc countedWord -> acc ++ ([show countedWord])) [] countedWordSet

toCountedWord :: String -> CountedWord
toCountedWord str = CountedWord str 1

countWordsInText :: String -> Set CountedWord
countWordsInText text = do
  let lines_ = lines text
      filteredLines = foldl (\acc l -> do
          let withoutPunctuations = filter (not . isPunctuation) l
          (acc ++ [withoutPunctuations]))
        []
        lines_

  let spaceFilteredLines = words $ unlines filteredLines

  findCountedWordsInStrings spaceFilteredLines

splitStringsByWords :: [String] -> [[String]]
splitStringsByWords strings = map words strings

findCountedWordsInStrings :: [String] -> Set CountedWord
findCountedWordsInStrings lines_  = do
  let words_ = splitStringsByWords lines_

  countWords words_

countWords :: [[String]] -> Set CountedWord
countWords wordsList = do
  foldl' (\acc word ->
      incrementWordCountInSet word acc
    ) Set.empty $
    concatMap words (concatMap id wordsList)

filterComparator :: [Char] -> (CountedWord -> Bool)
filterComparator wordToSearch = (\cw -> (map toUpper (word cw)) == (map toUpper wordToSearch))

getCountedWord :: String -> Set CountedWord -> Maybe CountedWord
getCountedWord wordToSearch set =
  if Set.size set == 0
    then
      Nothing
    else
      let filteredSet = Set.filter (filterComparator wordToSearch) set
      in find (filterComparator wordToSearch) (Set.toList filteredSet)

incrementWordCountInSet :: String -> Set CountedWord -> Set CountedWord
incrementWordCountInSet wordToIncrement set =
  let filteredSet = Set.filter (not . filterComparator wordToIncrement) set
      maybeCountedWord = getCountedWord wordToIncrement set
  in case maybeCountedWord of
       Just cw -> Set.insert (CountedWord (word cw) (count cw + 1)) filteredSet
       Nothing -> Set.insert (CountedWord wordToIncrement 1) set

writeToLocalTempFile :: [String] -> IO (FilePath)
writeToLocalTempFile strs = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines strs
  hClose tempHandle

  return tempName

-- ===================== TASK 04 =====================
