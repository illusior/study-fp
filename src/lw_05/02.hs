module Main where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Data.Char (isPunctuation)

-- Скопировать информацию из одного файла в другой, заменив знаки пунктуации заданным с клавиатуры символом.
-- Имена файлов указываются в командной строке.

data InputOutputFilePath = InputOutputFilePath {
  input :: String,
  output :: String
}

defaultOutputFileName :: String
defaultOutputFileName = "result.out"

showHello :: IO ()
showHello = do
  putStrLn "Программа скопирует содержимое одного файла, заменив знаки пунктуации на символ, заданный пользователем"
  putStrLn ""

showLackOfArgumentsError :: IO ()
showLackOfArgumentsError = do
  putStrLn "Пожалуйста, предоставьте путь до файла аргументом при запуске программы."
  putStrLn ""

  putStrLn "Примеры использования программы:"
  putStrLn ""

  putStrLn ("Пример №1 (файл-результат появится в каталоге, где запущена программа с названием" ++ defaultOutputFileName ++ "):")
  putStrLn "<executable> ./input.txt"
  putStrLn ""

  putStrLn ("Пример №2 (файл-результат появится в каталоге, где запущена программа с заданным названием:")
  putStrLn "<executable> ./input.txt ./output.txt"

readArgs :: IO (InputOutputFilePath)
readArgs = do
  args <- getArgs

  case length args of
    0 -> do
      showLackOfArgumentsError
      error "Error: Lack of arguments"
    1 -> return (InputOutputFilePath (args !! 0) defaultOutputFileName)
    _ -> return (InputOutputFilePath (args !! 0) (args !! 1))

main :: IO ()
main = do
  showHello

  ioFilePath <- readArgs

  let inputFilePath = input ioFilePath
  let outputFilePath = output ioFilePath

  putStrLn ("Путь до входного файла: " ++ inputFilePath)
  putStrLn ("Путь до файла-результата: " ++ outputFilePath ++ "\n")

  inputFileExists <- doesFileExist inputFilePath
  if not (inputFileExists) then do
    putStrLn "Входного файла не существует"
    error "Error: File does not exist"
  else do
    putStrLn "Введите символ, которым будут заменены все знаки пунктуации: "
    userCharReplacer <- getChar
    copyWithReplacePunctuationsFile inputFilePath outputFilePath userCharReplacer

replaceChar :: String -> Char -> String
replaceChar text newChar =
  map (\c -> if isPunctuation c then newChar else c) text

copyWithReplacePunctuationsFile :: String -> String -> Char -> IO ()
copyWithReplacePunctuationsFile inputFilePath outputFilePath newChar = do
  content <- readFile inputFilePath
  let newContent = replaceChar content newChar
  writeFile outputFilePath newContent
