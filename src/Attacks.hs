{-# LANGUAGE TemplateHaskell #-}

module Attacks (pawnAttacks, knightAttacks, kingAttacks) where

import Attacks.Internal
import BitBoard (BitBoard (BitBoard))
import Language.Haskell.TH.Syntax (lift)
import Square (Square)
import Piece (Color (..))

pawnAttacks :: Color -> Square -> BitBoard
pawnAttacks color square = BitBoard $ case color of
  Black -> $(lift (initSteppingAttacks [-7, -9])) !! fromEnum square
  White -> $(lift (initSteppingAttacks [7, 9])) !! fromEnum square

knightAttacks :: Square -> BitBoard
knightAttacks square = BitBoard $ $(lift (initSteppingAttacks [17, 15, 10, 6, -17, -15, -10, -6])) !! fromEnum square

kingAttacks :: Square -> BitBoard
kingAttacks square = BitBoard $ $(lift (initSteppingAttacks [9, 8, 7, 1, -9, -8, -7, -1])) !! fromEnum square
