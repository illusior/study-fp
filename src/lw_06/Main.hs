{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Data.Char (isDigit)
import TagGame.Board as TagGameBoard
  ( Game (..),
    getStatistic,
    initGame,
    isGameOver,
    move,
    shuffle,
    updateWonStatus,
  )
import TagGame.Move as TagGameMove (Move (..))
import TagGame.Statistic (Statistic (..))
import Text.Printf (printf)

main :: IO ()
main = play

data Query = Quit | NewGame Int | Play Move | ShowStats

-- Основные функции

play :: IO ()
play = do
  let gamesHistory = [] :: [Statistic]
  game <- greetings >> setup
  gameLoop game gamesHistory

setup :: IO Game
setup =
  putStrLn "Начнём новую игру?"
    >> putStrLn "Укажите сложность (положительное целое число): "
    >> getLine
    >>= maybe setup shuffle . readInt

gameLoop :: Game -> [Statistic] -> IO ()
gameLoop game gamesHistory
  | isGameOver game = do
      let updatedGame = updateWonStatus game True
      newGame <- (showStats updatedGame >> setup)
      let newGamesHistory = gamesHistory ++ [getStatistic updatedGame]
      gameLoop newGame newGamesHistory
  | otherwise = showGame game >> askForMove >>= reactOnMove game gamesHistory

-- Запросы пользователя

reactOnMove :: Game -> [Statistic] -> Query -> IO ()
reactOnMove game gamesHistory query = case query of
  Quit -> quit
  NewGame difficulty -> do
    startNewGameMsg difficulty
    newGame <- shuffle difficulty
    gameLoop newGame gamesHistory
  Play playerMove -> do
    let updGame = move playerMove game
    gameLoop updGame gamesHistory
  ShowStats -> showGameHistory gamesHistory >> (gameLoop game gamesHistory)

askForMove :: IO Query
askForMove =
  showAsk
    >> getLine
    >>= maybe askAgain return . parseQuery
  where
    askAgain = wrongMove >> askForMove

parseQuery :: String -> Maybe Query
parseQuery x = case x of
  "up" -> Just $ Play Up
  "u" -> Just $ Play Up
  "down" -> Just $ Play Down
  "d" -> Just $ Play Down
  "left" -> Just $ Play TagGameMove.Left
  "l" -> Just $ Play TagGameMove.Left
  "right" -> Just $ Play TagGameMove.Right
  "r" -> Just $ Play TagGameMove.Right
  "stat" -> Just $ ShowStats
  "s" -> Just $ ShowStats
  "quit" -> Just $ Quit
  "q" -> Just $ Quit
  'n' : 'e' : 'w' : ' ' : n -> Just . NewGame =<< readInt n
  'n' : ' ' : n -> Just . NewGame =<< readInt n
  _ -> Nothing

readInt :: String -> Maybe Int
readInt n
  | all isDigit n = Just $ read n
  | otherwise = Nothing

-- Ответы пользователю

greetings :: IO ()
greetings =
  putStrLn "Привет! Это игра пятнашки"
    >> showGame initGame
    >> remindMoves

startNewGameMsg :: Int -> IO ()
startNewGameMsg difficultyLevel = do
  putStrLn "Начата новая игра!"
  printf "Уровень сложности: %d" difficultyLevel
  putStrLn ""

showGame :: Game -> IO ()
showGame = putStrLn . show

wrongMove :: IO ()
wrongMove = putStrLn "Не могу распознать ход." >> remindMoves

showAsk :: IO ()
showAsk = putStrLn "Ваш ход: "

remindMoves :: IO ()
remindMoves = mapM_ putStrLn talk
  where
    talk =
      [ "",
        "Возможные ходы пустой клетки:",
        "   left     или l       -- налево",
        "   right    или r       -- направо",
        "   up       или u       -- вверх",
        "   down     или d       -- вниз",
        "",
        "Другие действия:",
        "   new int  или n int   -- начать новую игру, int - целое число,",
        "                                              указывающее на сложность",
        "   stat     или s       -- показать статистику",
        "   quit     или q       -- выход из игры",
        ""
      ]

showStats :: Game -> IO ()
showStats (Game _ _ stats) = print stats

showGameHistory :: [Statistic] -> IO ()
showGameHistory stats = do
  let result =
        ( foldl
            ( \allStats (i, stat) ->
                allStats
                  ++ "\nИгра #"
                  ++ show (i + 1)
                  ++ ": "
                  ++ show stat
            )
            ""
            (zip [0 ..] stats)
        )
  putStrLn "Статистика предыдущих игр:"
  printf result

quit :: IO ()
quit = putStrLn "До встречи." >> return ()
