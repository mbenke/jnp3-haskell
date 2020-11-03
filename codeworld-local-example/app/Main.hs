module Main where
import CodeWorld

import Lib

main :: IO ()
--  main = someFunc
main =  drawingOf(design)
design = circle 2 & rectangle 2 2