{-# LANGUAGE NumericUnderscores #-}

module Board (attackersTo, standard, Board (..), legalMoves, Move (..), move, fen) where

import Attacks
import BitBoard
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)
import Square
import Types

data Role = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Move
  = NormalMove
      { normalRole :: Role,
        normalFrom :: Square,
        normalTo :: Square,
        normalCapture :: Maybe Role
      }
  | EnPassantMove Square Square
  deriving (Show)

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

fen :: Board -> String
fen board = concatMap withRank (reverse $ enumFromTo Rank1 Rank8)
  where
    withRank rank =
      (if rank /= Rank8 then "/" else "")
        ++ if count /= 0 then acc ++ show count else acc
      where
        (count, acc) = foldr zipper (0, []) (enumFromTo FileA FileH)
        zipper file (count, acc) = case pieceAt (square file rank) board of
          Just piece ->
            let s = if count /= 0 then show count else ""
             in (0, acc ++ s ++ [pieceChar piece])
          Nothing -> (count + 1, acc)

standard :: Board
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
  NormalMove role from to capture ->
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

attackersTo :: Square -> Color -> Board -> BitBoard
attackersTo square attacker board =
  let BitBoard pawnAttackers = pawnAttacks attacker square
      BitBoard pawns = getPawns board
      BitBoard knightAttackers = knightAttacks square
      BitBoard knights = getKnights board
   in BitBoard $ (pawnAttackers .&. pawns) .|. (knightAttackers .&. knights)

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

byColor :: Color -> Board -> BitBoard
byColor Black = getBlack
byColor White = getWhite

-- Returns the pieces of the current player
us :: Board -> BitBoard
us board = byColor (getTurn board) board

-- Returns the pieces of the next player
them :: Board -> BitBoard
them board = byColor (toggle $ getTurn board) board

pawnMoves :: BitBoard -> Board -> [Move]
pawnMoves target board =
  let (east, west) = case getTurn board of
        Black -> (SouthEast, SouthWest)
        White -> (NorthEast, NorthWest)

      -- TODO promotions
      pawnCaptures dir =
        let backRanks = BitBoard 0xff00_0000_0000_00ff
            captures = translate dir (us board .&. getPawns board) .&. them board .&. target
            f to =
              NormalMove
                { normalRole = Pawn,
                  normalTo = to,
                  normalFrom = toEnum (fromEnum to - offset dir),
                  normalCapture = roleAt to board
                }
         in map f (squares $ captures .&. backRanks)
   in pawnCaptures east ++ pawnCaptures west

knightMoves :: BitBoard -> Board -> [Move]
knightMoves (BitBoard target) board = concatMap f (squares $ getKnights board)
  where
    f from =
      map
        ( \to ->
            NormalMove
              { normalRole = Knight,
                normalFrom = from,
                normalTo = to,
                normalCapture = roleAt to board
              }
        )
        (squares $ BitBoard $ target .&. attacks)
      where
        BitBoard attacks = knightAttacks from

legalMoves :: Board -> [Move]
legalMoves board =
  let bb = (complement $ us board)
   in pawnMoves bb board ++ knightMoves bb board
