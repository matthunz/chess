{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Color = Black | White deriving (Show)

data Role = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Move = NormalMove Role Square Square | EnPassantMove Square Square

data Board = Board
  { getTurn :: Color,
    getPawns :: BitBoard,
    getKnights :: BitBoard,
    getBishops :: BitBoard,
    getRooks :: BitBoard,
    getQueens :: BitBoard,
    getKings :: BitBoard,
    getBlack :: BitBoard,
    getWhite :: BitBoard,
    getOccupied :: BitBoard
  }
  deriving (Show)

standard =
  Board
    White
    (BitBoard 0x00ff_0000_0000_ff00)
    (BitBoard 0x4200_0000_0000_0042)
    (BitBoard 0x2400_0000_0000_0024)
    (BitBoard 0x8100_0000_0000_0081)
    (BitBoard 0x0800_0000_0000_0008)
    (BitBoard 0x1000_0000_0000_0010)
    (BitBoard 0xffff_0000_0000_0000)
    (BitBoard 0xffff)
    (BitBoard 0xffff_0000_0000_ffff)

move :: Move -> Board -> Board
move m board = case m of
  NormalMove role from to ->
    moveRole $ moveColor board
    where
      moveRole board = case role of
        Pawn -> board {getPawns = mv $ getPawns board}
        Knight -> board {getKnights = mv $ getKnights board}
        Bishop -> board {getBishops = mv $ getBishops board}
        Rook -> board {getRooks = mv $ getRooks board}
        Queen -> board {getQueens = mv $ getQueens board}
        King -> board {getKings = mv $ getKings board}
      moveColor board = case getTurn board of
        Black -> board {getTurn = White, getBlack = mv $ getBlack board}
        White -> board {getTurn = Black, getWhite = mv $ getWhite board}
      mv bb = moveSquare bb from to
  EnPassantMove from to -> error "TODO"

roleAt :: Square -> Board -> Maybe Role
roleAt square board
  | check getPawns = Just Pawn
  | check getKnights = Just Knight
  | check getBishops = Just Bishop
  | check getRooks = Just Rook
  | check getQueens = Just Queen
  | check getKings = Just King
  | otherwise = Nothing
  where
    check f = contains square (f board)

data Piece = Piece Color Role

pieceAt :: Square -> Board -> Maybe Piece
pieceAt square board = case roleAt square board of
  Just role -> Just $ Piece color role
  Nothing -> Nothing
  where
    color = if contains square $ getBlack board then Black else White

newtype BitBoard = BitBoard Word64 deriving (Show)

fromSquare :: Square -> BitBoard
fromSquare square = BitBoard $ 1 `shiftL` fromEnum square

fromRank :: Rank -> BitBoard
fromRank rank = BitBoard $ 0xff `shiftL` (fromEnum rank * 8)

fromFile :: File -> BitBoard
fromFile file = BitBoard $ 0x0101_0101_0101_0101 `shiftL` fromEnum file

contains :: Square -> BitBoard -> Bool
contains square (BitBoard bb) = (bb .&. rhs) /= 0
  where
    BitBoard rhs = fromSquare square

moveSquare :: BitBoard -> Square -> Square -> BitBoard
moveSquare (BitBoard bb) fromSquare toSquare =
  let movedBit = shiftL 1 (fromEnum toSquare)
      clearedFromBit = bb .&. complement (shiftL 1 (fromEnum fromSquare))
   in BitBoard (clearedFromBit .|. movedBit)

data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH deriving (Show, Eq, Enum)

data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8 deriving (Show, Eq, Enum)

data Square
  = A1
  | B1
  | C1
  | D1
  | E1
  | F1
  | G1
  | H1
  | A2
  | B2
  | C2
  | D2
  | E2
  | F2
  | G2
  | H2
  | A3
  | B3
  | C3
  | D3
  | E3
  | F3
  | G3
  | H3
  | A4
  | B4
  | C4
  | D4
  | E4
  | F4
  | G4
  | H4
  | A5
  | B5
  | C5
  | D5
  | E5
  | F5
  | G5
  | H5
  | A6
  | B6
  | C6
  | D6
  | E6
  | F6
  | G6
  | H6
  | A7
  | B7
  | C7
  | D7
  | E7
  | F7
  | G7
  | H7
  | A8
  | B8
  | C8
  | D8
  | E8
  | F8
  | G8
  | H8
  deriving (Show, Eq, Enum)

square :: File -> Rank -> Square
square file rank = toEnum (fromEnum file .|. (fromEnum rank `shiftL` 3))
