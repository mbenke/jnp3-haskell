module Main where
import Atom
import Lens7

point :: Lens Atom Point
point = mkLens _point setPoint

element :: Lens Atom String
element = mkLens _element setElement

x, y :: Lens Point Double
x = mkLens _x setX
y = mkLens _y setY

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)

atom2 = moveAtom atom0

main = mapM_ print [atom0, atom1, atom2]
