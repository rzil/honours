module Types where

type Coord = Float
data Box = Box {left :: Coord, right :: Coord, up :: Coord, down :: Coord, kind :: Maybe String, label :: Maybe Int}  deriving Show

defaultBox = Box {left = 0, right = 0, up = 0, down = 0, kind = Nothing, label = Nothing}
