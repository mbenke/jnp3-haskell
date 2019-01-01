module Lens3 where

data Atom = Atom { _element :: String, _point :: Point }
data Point = Point { _x :: Double, _y :: Double }

setPoint :: Point -> Atom -> Atom
setPoint p a = a { _point = p }

setElement :: String -> Atom -> Atom
setElement e a = a { _element = e }

setX, setY:: Double -> Point -> Point
setX x p = p { _x = x }
setY y p = p { _y = y }

data Lens a b = Lens { view :: a -> b
--                     , set :: b -> a -> a
                     , over :: (b -> b) -> (a -> a)
                     }
mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view over
  where over f a = set (f (view a)) a

point :: Lens Atom Point
point = mkLens _point setPoint

element :: Lens Atom String
element = mkLens _element setElement

x, y :: Lens Point Double
x = mkLens _x setX
y = mkLens _y setY

comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens { view = (view l2 . view l1)
--                  , set = (\c -> over l1 (set l2 c))
                  , over = (over l1 . over l2)
                  }
             

set :: Lens a b -> b -> a -> a
set l x = over l (const x)

{-
askX :: Atom -> IO Atom
askX a = over (point `comp` x) askUser a
  where
    askUser :: Double -> IO Double
    askUser x = do
	putStrLn $ "Current position is " ++ show x ++ ". New Position?"
	answer <- getLine
	return (read answer)
-}
