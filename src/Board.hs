{-# LANGUAGE NumericUnderscores #-}

module Board
  ( Board (..),

    -- * Constructors
    standard,

    -- * Bitboards
    byColor,
    byRole,
    us,
    them,
    our,
    attackersTo,
    checkers,

    -- * Serialization
    fen,

    -- * Moves
    move,
    moves,

    -- * Pieces
    roleAt,
    pieceAt,
    Move (..),
  )
where

import Attacks
import BitBoard
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)
import Piece
import Square

-- | Chess board
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

-- | Output the FEN text of a board
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

-- | Create a board with the standard starting position
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

-- | Returns the attackers of a color to the given square
attackersTo :: Square -> Color -> Board -> BitBoard
attackersTo square attacker board =
  (pawnAttacks attacker square .&. getPawns board) <> (knightAttacks square .&. getKnights board)

-- | Returns the attackers to the current player's king
checkers :: Board -> BitBoard
checkers board = case squares $ our King board of
  [] -> BitBoard 0
  (sq : _) -> attackersTo sq (toggle $ getTurn board) board

-- | Returns the role of the piece at `square` or `Nothing` if there is none.
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

-- | Returns the piece at `square` or `Nothing` if there is none.
pieceAt :: Square -> Board -> Maybe Piece
pieceAt square board =
  let color = if contains square $ getBlack board then Black else White
   in Piece color <$> roleAt square board

byColor :: Color -> Board -> BitBoard
byColor Black = getBlack
byColor White = getWhite

byRole :: Role -> Board -> BitBoard
byRole role = case role of
  Pawn -> getPawns
  Knight -> getKnights
  Bishop -> getBishops
  Rook -> getRooks
  Queen -> getQueens
  King -> getKings

-- | Returns the pieces of the current player
us :: Board -> BitBoard
us board = byColor (getTurn board) board

-- | Returns the pieces of the next player
them :: Board -> BitBoard
them board = byColor (toggle $ getTurn board) board

-- | Returns the pieces of the current player with the given role.
our :: Role -> Board -> BitBoard
our role board = us board .&. byRole role board

-- | Move to perform on a chess board
data Move
  = NormalMove
      { normalRole :: Role,
        normalFrom :: Square,
        normalTo :: Square,
        normalCapture :: Maybe Role
      }
  | EnPassantMove Square Square
  deriving (Show)

-- | Perform a move on the board
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

pawnMoves :: BitBoard -> Board -> [Move]
pawnMoves target board =
  let (east, west) = case getTurn board of
        Black -> (SouthEast, SouthWest)
        White -> (NorthEast, NorthWest)

      -- TODO promotions
      pawnCaptures dir =
        let backRanks = BitBoard 0xff00_0000_0000_00ff
            captures = translate dir (us board .&. getPawns board) .&. them board .&. target
            moveTo to =
              NormalMove
                { normalRole = Pawn,
                  normalTo = to,
                  normalFrom = toEnum (fromEnum to - offset dir),
                  normalCapture = roleAt to board
                }
         in map moveTo (squares $ captures .&. backRanks)
   in pawnCaptures east ++ pawnCaptures west

stepperMoves :: Role -> (Square -> BitBoard) -> BitBoard -> Board -> [Move]
stepperMoves role getAttacks target board = concatMap withFrom (squares $ byRole role board)
  where
    withFrom from =
      let withTo to =
            NormalMove
              { normalRole = role,
                normalFrom = from,
                normalTo = to,
                normalCapture = roleAt to board
              }
       in map withTo (squares $ target .&. getAttacks from)

knightMoves :: BitBoard -> Board -> [Move]
knightMoves = stepperMoves Knight knightAttacks

kingMoves :: BitBoard -> Board -> [Move]
kingMoves = stepperMoves Knight kingAttacks

-- | Generate all legal moves
moves :: Board -> [Move]
moves board =
  let bb = (complement $ us board)
   in pawnMoves bb board ++ knightMoves bb board ++ kingMoves bb board
