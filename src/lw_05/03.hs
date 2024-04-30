module Main where

import System.Environment (getArgs)
import System.Directory (removeFile, renameFile)
import System.IO
    (hClose,
      hGetContents,
      hPutStr,
      openTempFile,
      IOMode(ReadMode))
import Data.List (delete)
import Data.Char (toUpper, toLower)
import GHC.IO.Handle.FD (openFile)

-- Программа работы с файлом предусматривает: просмотр содержимого, добавление новой информации, удаление какой-либо строки,
-- копирование содержимого в новый файл с использованием двух видов фильтрации (фильтр выбираем самостоятельно).

-- Имена исходных файлов задаются в командной строке,
-- вид работы с файлом вводится с клавиатуры, также, как и дополнительная информация (напр. строка ввода).

addCommand :: String
addCommand = "add"

copyCommand :: String
copyCommand = "copy"

viewCommand :: String
viewCommand = "view"

removeCommand :: String
removeCommand = "remove"

helpCommand :: String
helpCommand = "help"

dispatch :: [(String, [String] -> IO ())]
dispatch = [ (addCommand, add)
           , (copyCommand, copy)
           , (viewCommand, view)
           , (removeCommand, remove)
           , (helpCommand, help)
           ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [] = do
  showNeedHelpMsgForCmnd addCommand
add [_] = add []
add (_:_:_:_) = add []
add [fileName, toAppend] = appendFile fileName (toAppend ++ "\n")

copy :: [String] -> IO ()
copy [] = do
  showNeedHelpMsgForCmnd copyCommand
copy [_] = copy []
copy [_, _] = copy []
copy (_:_:_:_:_) = copy []
copy [fileName, toFileName, copyFilterName] = do
  handle <- openFile fileName ReadMode
  contents <- (hGetContents handle)

  let (Just chosenFilter) = lookup copyFilterName dispatchCopyFilter

  let newContents = chosenFilter (lines contents)

  tempName <- writeToLocalTempFile newContents
  renameFile tempName toFileName

  hClose handle

view :: [String] -> IO ()
view [] = do
  showNeedHelpMsgForCmnd viewCommand
view (_:_:_) = view []
view [fileName] = do
    handle <- openFile fileName ReadMode
    contents <- (hGetContents handle)

    let line = lines contents
        numberedLines = zipWith (\n l -> (show n ++ " - " ++ l)) [0..] line

    putStr $ unlines numberedLines
    hClose handle

remove :: [String] -> IO ()
remove [] = do
  showNeedHelpMsgForCmnd removeCommand
  return ()
remove [_] = remove []
remove (_:_:_:_) = remove []
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    contents <- (hGetContents handle)

    let number = read numberString
        fileLines = lines contents
        newLines = delete (fileLines !! number) fileLines

    tempName <- writeToLocalTempFile newLines
    hClose handle

    removeFile fileName
    renameFile tempName fileName

writeToLocalTempFile :: [String] -> IO (FilePath)
writeToLocalTempFile strs = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ unlines strs
  hClose tempHandle

  return tempName

help :: [String] -> IO ()
help [] = do
  putStrLn "HELP:"
  putStrLn ""

  putStrLn (map toUpper addCommand ++ ":")
  putStrLn "Чтобы добавить содержимое в файл, используйте следующую сигнатуру вызова программы:"
  putStrLn ("<exe> " ++ addCommand ++ " <путь_до_файла> \"то, что хотите добавить\"")
  putStrLn ""

  putStrLn (map toUpper copyCommand ++ ":")
  putStrLn "Чтобы копировать содержимое файла, используйте следующую сигнатуру вызова программы:"
  putStrLn ("<exe> " ++ copyCommand ++ " <путь_до_файла> <путь_до_файла_new> <фильтр_при_копировании>")
  putStrLn ""
  putStrLn "<фильтр_при_копировании> - будет определять, как будет копироваться информация"
  putStrLn ("фильтр_при_копировании = " ++ toUpperCopyFilter ++ " - приведёт весь текст к верхнему регистру")
  putStrLn ("                       | " ++ toLowerCopyFilter ++ " - приведёт весь текст к нижнему регистру")
  putStrLn ""

  putStrLn (map toUpper viewCommand ++ ":")
  putStrLn "Чтобы просмотреть содержимое файла, используйте следующую сигнатуру вызова программы:"
  putStrLn ("<exe> " ++ viewCommand ++ " <путь_до_файла>")
  putStrLn ""

  putStrLn (map toUpper removeCommand ++ ":")
  putStrLn "Чтобы удалить строку из файла, используйте следующую сигнатуру вызова программы:"
  putStrLn ("<exe> " ++ removeCommand ++ " <путь_до_файла> <номер_строки>")
  putStrLn ""
help (_:_) = help []

showNeedHelpMsgForCmnd :: String -> IO ()
showNeedHelpMsgForCmnd wrongCommand = do
  putStrLn ("Неверное использование команды '" ++ wrongCommand ++ "'")
  putStrLn ""
  putStrLn "Ознакомьтесь с возможностями программы, вызвав меню помощи"
  putStrLn ("<exe> " ++ helpCommand)
  putStrLn ""


toUpperCopyFilter :: String
toUpperCopyFilter = "toUpper"

toLowerCopyFilter :: String
toLowerCopyFilter = "toLower"

dispatchCopyFilter :: [(String, [String] -> [[Char]])]
dispatchCopyFilter = [ (toUpperCopyFilter, toUpperStrings)
                     , (toLowerCopyFilter, toLowerStrings)
                     ]

toUpperStrings :: [String] -> [[Char]]
toUpperStrings strs = map (map toUpper) strs

toLowerStrings :: [String] -> [[Char]]
toLowerStrings strs = map (map toLower) strs
