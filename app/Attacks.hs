{-# LANGUAGE TemplateHaskell #-}


module Attacks (pawnAttacks) where

import BitBoard (BitBoard (BitBoard))
import Board (Color (..))
import Data.Int (Int64)
import Data.Word (Word64)
import Language.Haskell.TH.Syntax (lift)
import Square (Square)
import Attacks.Internal

pawnAttacks :: Color -> Square -> BitBoard
pawnAttacks color square = BitBoard $ case color of
  Black -> $(lift (initSteppingAttacks [7, 9])) !! fromEnum square
  White -> $(lift (initSteppingAttacks [-7, -9])) !! fromEnum square
