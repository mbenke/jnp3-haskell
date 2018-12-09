module Records where

data Coord = C { cx :: Integer, cy :: Integer } deriving (Eq, Show)
data MazeMap -- stub
data Direction = L | U | R | D deriving (Eq, Show)

data State = S {
  stPlayer :: Coord,
  stDir    :: Direction,
  stBoxes  :: [Coord],
  stMap    :: MazeMap,
  stXdim   :: [Integer],
  stYdim   :: [Integer],
  stLevel  :: Integer,
  stMove   :: Integer
  }

foo s = s { stDir = D, stMove = stMove s + 1  }

data Alt = A { a :: Int } | B { b :: Int }
