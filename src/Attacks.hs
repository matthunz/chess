{-# LANGUAGE TemplateHaskell #-}

module Attacks (pawnAttacks, knightAttacks) where

import Attacks.Internal
import BitBoard (BitBoard (BitBoard))
import Data.Int (Int64)
import Data.Word (Word64)
import Language.Haskell.TH.Syntax (lift)
import Square (Square)
import Types (Color (..))

pawnAttacks :: Color -> Square -> BitBoard
pawnAttacks color square = BitBoard $ case color of
  Black -> $(lift (initSteppingAttacks [7, 9])) !! fromEnum square
  White -> $(lift (initSteppingAttacks [-7, -9])) !! fromEnum square

knightAttacks :: Square -> BitBoard
knightAttacks square = BitBoard $ $(lift (initSteppingAttacks [17, 15, 10, 6, -17, -15, -10, -6])) !! fromEnum square
