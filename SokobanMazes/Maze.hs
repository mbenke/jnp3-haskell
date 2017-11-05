module Maze where

data Maze = Maze Coord (Coord -> Tile) 

data Coord = C Integer Integer deriving (Eq, Show)
data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq, Show)
