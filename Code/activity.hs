{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO () -- program wykonuje IO i nie daje wartości 

main :: Program
main = program

program = activityOf start handleEvent draw

start = (0,0)
draw (x,y) = translated x y (solidCircle 0.5) & coordinatePlane

handleEvent (KeyPress key) (x, y)
    | key == "Right" = (x+1,y)
    | key == "Up"    = (x,y+1)
    | key == "Left"  = (x-1,y)
    | key == "Down"  = (x,y-1)
handleEvent _ c      = c
