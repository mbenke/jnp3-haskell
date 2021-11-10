module JKKMazes where
import Maze

-- Wariant domyślnego, gdzie jest więcej Storage niz Boxów, a plansza jest lekko większa.
initmaze2 :: Maze
initmaze2 = Maze start map where
  start = C 0 1
  map (C x y)
   | abs x > 5  || abs y > 5      = Blank  -- blank
   | abs x == 5 || abs y == 5     = Wall  -- wall
   | x ==  2 && y <= 0            = Wall  -- wall
   | x ==  3 && y <= 0            = Storage  -- storage
   | x >= -3 && x <= 0 && y == 0  = Box  -- box
   | otherwise                    = Ground  -- ground

-- Brak większości ścian.
badmaze1 :: Maze
badmaze1 = Maze start map where
  start = C 0 1
  map (C x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | x ==  2 && y <= 0        = Wall  -- wall
   | x ==  3 && y <= 0        = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground

-- Za dużo boxów.
badmaze2 :: Maze
badmaze2 = Maze start map where
  start = C 0 1
  map (C x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | abs x == 4 || abs y == 4 = Wall  -- wall
   | x ==  3 && y <= 0        = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground

-- Za dużo boxów.
badmaze3 :: Maze
badmaze3 = Maze start map where
  start = C 0 1
  map (C x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | abs x == 4 || abs y == 4 = Wall  -- wall
   | x ==  2 && y <= 0        = Wall  -- wall
   | x ==  3 && y <= (-1)     = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground
