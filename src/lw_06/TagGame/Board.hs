module TagGame.Board
  ( Board,
    Game (..),
    Label,
    Pos,
    allGamesStats,
    getStatistic,
    initGame,
    isGameOver,
    move,
    shuffle,
    updateWonStatus,
  )
where

import Data.Array
  ( Array,
    listArray,
    (!),
    (//),
  )
import System.Random (randomRIO)
import TagGame.Move as TagGameMove
  ( Move (..),
    orient,
  )
import TagGame.Statistic as TagGameStatistics (Statistic (..))
import TagGame.Vector as TagGameVector
  ( Pos,
    shift,
    within,
  )

type Board = Array Pos Label

type Label = Int

data Game = Game
  { emptyField :: Pos,
    gameBoard :: Board,
    statistic :: Statistic
  }
  deriving (Eq)

--  +----+----+----+----+
--  |  1 |  2 |  3 |  4 |
--  +----+----+----+----+
--  |  5 |  6 |  7 |  8 |
--  +----+----+----+----+
--  |  9 | 10 | 11 | 12 |
--  +----+----+----+----+
--  | 13 | 14 | 15 |    |
--  +----+----+----+----+

instance Show Game where
  show :: Game -> String
  show (Game _ board (Statistic _ __)) =
    "\n"
      ++ space
      ++ line
      ++ (foldr (\a b -> a ++ space ++ line ++ b) "\n" $ map column [0 .. 3])
    where
      post _id = showLabel $ board ! _id
      showLabel n = cell $ show $ case n of
        15 -> 0
        _n -> n + 1
      cell [] = "    "
      cell (_ : _ : _ : _) = cell []
      cell "0" = cell []
      cell [x] = ' ' : ' ' : x : ' ' : []
      cell [a, b] = ' ' : a : b : ' ' : []
      line = "+----+----+----+----+\n"
      nums =
        ((space ++ "|") ++)
          . foldr (\a b -> a ++ "|" ++ b) "\n"
          . map post
      column i = nums $ map (\x -> (i, x)) [0 .. 3]
      space = "\t"

emptyLabel :: Label
emptyLabel = 15

move :: Move -> Game -> Game
move m (Game oldEmptyPos board (Statistic moves hasWon))
  | within newEmptyPos = Game newEmptyPos (board // updates) (Statistic (moves + 1) hasWon)
  | otherwise = Game oldEmptyPos board (Statistic moves hasWon)
  where
    newEmptyPos = shift (orient m) oldEmptyPos
    updates = [(oldEmptyPos, board ! newEmptyPos), (newEmptyPos, emptyLabel)]

allGamesStats :: [Statistic]
allGamesStats = []

isGameOver :: Game -> Bool
isGameOver (Game _ board _) = board == initBoard

initStatistic :: Statistic
initStatistic =
  Statistic
    { moveCount = 0,
      hasWon = False
    }

initBoard :: Board
initBoard = listArray ((0, 0), (3, 3)) [0 .. 15]

initGame :: Game
initGame =
  Game
    { emptyField = (3, 3),
      gameBoard = initBoard,
      statistic = initStatistic
    }

shuffling :: Game -> IO Game
shuffling g = flip move g <$> (randomElem $ nextMoves g)

updateStatistic :: Game -> Statistic -> Game
updateStatistic game newStat = game {statistic = newStat}

updateWonStatus :: Game -> Bool -> Game
updateWonStatus game newStat =
  game
    { statistic =
        (statistic game)
          { hasWon = newStat
          }
    }

shuffle :: Int -> IO Game
shuffle n = do
  game <- (iterate (shuffling =<<) $ pure initGame) !! n -- TODO: game is setting n as moveCount in stats
  return (updateStatistic game initStatistic)

randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)

nextMoves :: Game -> [Move]
nextMoves g = filter (within . moveEmptyTo . orient) allMoves
  where
    moveEmptyTo v = shift v (emptyField g)
    allMoves = [Up, Down, TagGameMove.Left, TagGameMove.Right]

getStatistic :: Game -> Statistic
getStatistic (Game _ _ s) = s
