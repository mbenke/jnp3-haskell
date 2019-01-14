module Main where
import Atom
import Lens3

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)

atom2 = moveAtom atom0

main = mapM_ print [atom0, atom1, atom2]
