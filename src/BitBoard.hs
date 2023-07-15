{-# LANGUAGE NumericUnderscores #-}

module BitBoard
  ( fromSquare,
    fromRank,
    fromFile,
    contains,
    moveSquare,
    squares,
    BitBoard (..),
  )
where

import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)
import Numeric (showBin)
import Square

newtype BitBoard = BitBoard Word64 deriving (Eq)

instance Bits BitBoard where
  (.&.) (BitBoard lhs) (BitBoard rhs) = BitBoard $ lhs .&. rhs
  (.|.) (BitBoard lhs) (BitBoard rhs) = BitBoard $ lhs .|. rhs
  xor (BitBoard lhs) (BitBoard rhs) = BitBoard $ lhs `xor` rhs
  complement (BitBoard bb) = BitBoard $ complement bb
  shift (BitBoard bb) n = BitBoard $ bb `shift` n
  rotate (BitBoard bb) n = BitBoard $ bb `rotate` n
  bitSizeMaybe (BitBoard bb) = bitSizeMaybe bb
  isSigned (BitBoard bb) = isSigned bb
  bit i = BitBoard $ bit i
  testBit (BitBoard bb) = testBit bb
  bitSize = error "Unimplemented!"
  popCount (BitBoard bb) = popCount bb

instance Semigroup BitBoard where
  (<>) (BitBoard lhs) (BitBoard rhs) = BitBoard $ lhs .|. rhs

instance Show BitBoard where
  show (BitBoard bb) = showBin bb ""

fromSquare :: Square -> BitBoard
fromSquare square = BitBoard $ 1 `shiftL` fromEnum square

fromRank :: Rank -> BitBoard
fromRank rank = BitBoard $ 0xff `shiftL` (fromEnum rank * 8)

fromFile :: File -> BitBoard
fromFile file = BitBoard $ 0x0101_0101_0101_0101 `shiftL` fromEnum file

isEmpty :: BitBoard -> Bool
isEmpty (BitBoard bb) = bb == 0

contains :: Square -> BitBoard -> Bool
contains square bb = not . isEmpty $ bb .&. fromSquare square

moveSquare :: BitBoard -> Square -> Square -> BitBoard
moveSquare (BitBoard bb) fromSquare toSquare =
  let movedBit = shiftL 1 (fromEnum toSquare)
      clearedFromBit = bb .&. complement (shiftL 1 (fromEnum fromSquare))
   in BitBoard (clearedFromBit .|. movedBit)

squares :: BitBoard -> [Square]
squares (BitBoard bb) = f 0 bb
  where
    f squareIndex b
      | squareIndex >= 64 = []
      | testBit b squareIndex = toEnum squareIndex : f (squareIndex + 1) b
      | otherwise = f (squareIndex + 1) b
