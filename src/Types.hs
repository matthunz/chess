module Types (Color (..), toggle) where

data Color = Black | White deriving (Show)

toggle :: Color -> Color
toggle Black = White
toggle White = Black