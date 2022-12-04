{-# LANGUAGE TemplateHaskell #-}

module GamePlay
  ( canMove
  ) where

import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head, set )
import Lens.Micro.TH (makeLenses)

import CardTypes
import Utils

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt
makeLenses ''Move

------------------------------------------------------------------------------- 


canMove :: GSt -> Move -> Bool
canMove _ _ = True
