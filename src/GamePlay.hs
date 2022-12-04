{-# LANGUAGE TemplateHaskell #-}

module GamePlay
  ( canMove
  ) where

import Data.Maybe (isJust, fromMaybe)
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


-- Constants Initialization

topOfPileIdx :: Int
topOfPileIdx = 0

-- Function to determine given a game state and an attempted move if it is possible

-- check for a draw Pile move

canMove :: GSt -> Move -> Bool

canMove gameState move
    | isfpiledraw && istpileplayer && hastpileidx && isdrawnotempty = True          -- Corresponds to player drawing a card from draw pile
    | otherwise                                                     = False
    where
        isfpiledraw     = move ^. fPileType == DrawP
        isfpileplayer   = move ^. fPileType == PlayerP
        isfpilecenter   = move ^. fPileType == CenterP
        isfpilecorner   = move ^. fPileType == CornerP
        istpileplayer   = move ^. tPileType == PlayerP
        istpilecenter   = move ^. tPileType == CenterP
        istpilecorner   = move ^. tPileType == CornerP
        iscardmove      = isJust (move ^. fCardIdx)
        hasfpileidx     = isJust (move ^. fPileIdx)
        hastpileidx     = isJust (move ^. tPileIdx)
        fcardidx        = fromMaybe topOfPileIdx (move ^. fCardIdx)
        fpileidx        = fromMaybe topOfPileIdx (move ^. fPileIdx)
        tpileidx        = fromMaybe topOfPileIdx (move ^. tPileIdx)
        isdrawnotempty  = not (null (gameState ^. field . draw . cards))
