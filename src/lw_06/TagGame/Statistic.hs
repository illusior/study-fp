module TagGame.Statistic where

type MoveCount = Int

data Statistic = Statistic
  { moveCount :: MoveCount,
    hasWon :: Bool
  }
  deriving (Eq)

instance Show Statistic where
  show :: Statistic -> String
  show (Statistic moveCount hasWon) =
    "Статистика:\n"
      ++ ("\tкол-во шагов: " ++ show moveCount ++ "\n")
      ++ ("\tстатус: " ++ (if hasWon then "победил" else "не победил") ++ "\n")