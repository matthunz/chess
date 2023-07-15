{-# LANGUAGE NumericUnderscores #-}

module BitBoard
  ( fromSquare,
    fromRank,
    fromFile,
    contains,
    moveSquare,
    BitBoard (..),
  )
where

import Data.Bits
import Data.Int (Int64)
import Data.Word (Word64)
import Square
import Numeric (showBin)

newtype BitBoard = BitBoard Word64

instance Show BitBoard where
  show (BitBoard bb) = showBin bb ""

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
