module Main where
import Atom
import Lens2

atom2 = moveAtom atom0

main = mapM_ print [atom0, atom1, atom2]
