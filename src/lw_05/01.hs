module Main where
import Data.List (isSuffixOf)
import Text.Read (readMaybe)

-- Вывести на экран сформированный список, данные для которого вводятся с клавиатуры:
-- начальное значение, количество элементов, кратность.

-- Например - [14, 21, 28]. 3 элемента списка, начинающиеся с 14, кратные 7.

sayHello :: IO ()
sayHello = do
  putStrLn "Программа генерирует список целых чисел, кратных заданному числу"
  putStrLn ""

main :: IO ()
main = do
  sayHello

  intStart <- readUserIntWithMessage "Введите число, с которого должен начинаться список" (\_ -> True) ""
  intCount <- readUserIntWithMessage "Введите длину списка" isPositive "Число должно быть положительным!"
  intMultiplier <- readUserIntWithMessage "Введите кратность" isPositive "Число должно быть положительным!"

  let adjustedInt = adjustInt intStart intMultiplier
  putStrLn ("" ++
      if adjustedInt == intStart
        then ""
        else "\n" ++ "Стартовое значение не кратно " ++ show intMultiplier ++ "!\n" ++ "Возьмём ближайшее кратное слева\n"
    )

  let result = generateIntList intStart intCount intMultiplier

  putStrLn ("Результат:" ++ "\n" ++ show result)

readUserIntWithMessage :: String -> (Int -> Bool) -> String -> IO Int
readUserIntWithMessage msg intValidator invalidMsg = do
  let sep = if (":" `isSuffixOf` msg) then "" else ":"

  putStrLn (msg ++ sep)
  input <- getLine

  case (readMaybe input :: Maybe Int) of
    Just n | intValidator n -> pure n
           | otherwise      -> do
            putStrLn invalidMsg
            readUserIntWithMessage msg intValidator invalidMsg
    Nothing -> do
      putStrLn "Неправильный формат ввода целого числа. Примеры: 1, 999, 0, -0, -999"
      readUserIntWithMessage msg intValidator invalidMsg

isPositive :: Int -> Bool
isPositive n = n > 0

generateIntList :: Int -> Int -> Int -> [Int]
generateIntList intStart intCount intMultiplier = do
  let adjustedStart = (adjustInt intStart intMultiplier) in
    [adjustedStart + (i * intMultiplier) | i <- [0..intCount - 1]]

adjustInt :: Int -> Int -> Int
adjustInt n multiple = n `div` multiple * multiple
