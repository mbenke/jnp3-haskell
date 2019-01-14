module Lens2 where
import Atom

data Lens a b = Lens { view :: a -> b
                     , set :: b -> a -> a
                     , over :: (b -> b) -> (a -> a)
                     }
mkLens :: (a -> b) -> (b -> a -> a) -> Lens a b
mkLens view set = Lens view set over
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
                  , set = (\c -> over l1 (set l2 c))
                  , over = (over l1 . over l2)
                  }


set' :: Lens a b -> b -> a -> a
set' l x = over l (const x)

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)
