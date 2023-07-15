{-# LANGUAGE NumericUnderscores #-}

module Board where

import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)

import BitBoard
import Square

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
