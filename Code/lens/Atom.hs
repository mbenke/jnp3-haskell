module Atom where

data Atom = Atom { _element :: String, _point :: Point } deriving Show
data Point = Point { _x :: Double, _y :: Double } deriving Show

setPoint :: Point -> Atom -> Atom
setPoint p a = a { _point = p }

setElement :: String -> Atom -> Atom
setElement e a = a { _element = e }

setX, setY:: Double -> Point -> Point
setX x p = p { _x = x }
setY y p = p { _y = y }

point0 = Point 0 0
point1 = Point 1 1

atom0 = Atom "H" point0
atom1 = Atom "O" point1
