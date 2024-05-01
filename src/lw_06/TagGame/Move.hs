module TagGame.Move ( Move(..)
                    , orient
                    ) where

import TagGame.Vector ( Vec(..) )

data Move = Up
          | Down
          | Left
          | Right
          deriving (Enum)

orient :: Move -> Vec
orient m = Vec $ case m of
    Up           -> (-1, 0)
    Down         -> (1 , 0)
    TagGame.Move.Left    -> (0 ,-1)
    TagGame.Move.Right   -> (0 , 1)
