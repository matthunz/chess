module Attacks.Internal (initSteppingAttacks) where

import BitBoard (BitBoard (..))
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.Int (Int32)
import Data.Word (Word64)

initSteppingAttacks :: [Int] -> [Word64]
initSteppingAttacks deltas = go 0 []
  where
    go 64 table = table
    go sq table =
      let attack = slidingAttacks sq (BitBoard $ complement 0) deltas
       in go (sq + 1) (table ++ [attack])

slidingAttacks :: Int -> BitBoard -> [Int] -> Word64
slidingAttacks square (BitBoard occupied) deltas = go 0 0
  where
    go i attack
      | i >= len = attack
      | otherwise =
          let previous = fromEnum square
              loop prev att
                | sq < 0 || sq > 63 || fileDiff > 2 || fileDiff < -2 = att
                | otherwise =
                    let bb = 1 `shiftL` sq
                        att' = att .|. bb
                     in if (occupied .&. bb) /= 0
                          then att'
                          else loop sq att'
                where
                  sq = prev + deltas !! i
                  fileDiff = (sq .&. 0x7) - (prev .&. 0x7)
           in go (i + 1) (loop previous attack)
    len = length deltas
