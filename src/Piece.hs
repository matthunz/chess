module Piece (Color (..), toggle, Role (..), Piece (..), pieceChar) where

data Color = Black | White deriving (Show)

toggle :: Color -> Color
toggle Black = White
toggle White = Black

--- Role of a piece
data Role = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

-- | Chess piece with a `Color` and `Role`
data Piece = Piece Color Role

-- | Returns the FEN character representation of the piece.
pieceChar :: Piece -> Char
pieceChar (Piece color role) = case color of
  Black ->
    case role of
      Pawn -> 'p'
      Knight -> 'n'
      Bishop -> 'b'
      Rook -> 'r'
      Queen -> 'q'
      King -> 'k'
  White ->
    case role of
      Pawn -> 'P'
      Knight -> 'N'
      Bishop -> 'B'
      Rook -> 'R'
      Queen -> 'Q'
      King -> 'K'