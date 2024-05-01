module TagGame.Vector ( Vec(..)
                      , Pos
                      , within
                      , shift
                      ) where

newtype Vec = Vec (Int, Int)

type Pos = (Int, Int)

within :: Pos -> Bool
within (a, b) = p a && p b
    where p x = x >= 0 && x <= 3

shift :: Vec -> Pos -> Pos
shift (Vec (va, vb)) (pa, pb) = (va + pa, vb + pb)
