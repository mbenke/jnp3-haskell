module Lens1 where
import Atom

getAtomX :: Atom -> Double
getAtomX = _x . _point

-- setAtomX :: Double -> Atom -> Atom
-- setAtomX x a = setPoint (setX x (_point a)) a

data Lens a b = Lens { view :: a -> b
                     , set :: b -> a -> a
                     }

point :: Lens Atom Point
point = Lens _point setPoint

element :: Lens Atom String
element = Lens _element setElement

x, y :: Lens Point Double
x = Lens _x setX
y = Lens _y setY


-- comp :: Lens a b -> Lens b c -> Lens a c
-- comp l1 l2 = Lens (view l2 . view l1)
--                   (\c a -> set l1 (set l2 c (view l1 a)) a)

setAtomX :: Double -> Atom -> Atom
setAtomX = set (point `comp` x)

over :: Lens a b -> (b -> b) -> (a -> a)
over l f a = set l (f (view l a)) a

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)


comp :: Lens a b -> Lens b c -> Lens a c
comp l1 l2 = Lens (view l2 . view l1)
                  (\c -> over l1 (set l2 c))
