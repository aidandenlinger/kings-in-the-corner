{-# LANGUAGE TemplateHaskell #-}

module GamePlay
  ( canMove,
    getScore
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

-- Helper functions to get cards

getPlayerCard :: GSt -> Int -> Int -> Card
getPlayerCard gameState pileidx cardidx = (((getPHands gameState !! pileidx) ^. cards) !! cardidx) ^. card

getCenterTop :: GSt -> Int -> Card
getCenterTop gameState pileidx = ((((gameState ^. field . center) !! pileidx) ^. cards) !! topOfPileIdx) ^. card

getCenterBottom :: GSt -> Int -> Card
getCenterBottom gameState pileidx = last (((gameState ^. field . center) !! pileidx) ^. cards) ^. card

getCornerTop :: GSt -> Int -> Card
getCornerTop gameState pileidx = ((((gameState ^. field . corner) !! pileidx) ^. cards) !! topOfPileIdx) ^. card


-- Helper functions to check for move validity

isNextCard :: Card -> Card -> Bool
isNextCard (Card rf sf) (Card rt st) = (succ rf == rt) && (assignColor sf /= assignColor st)

checkCenterMove :: GSt -> Int -> Card -> Bool
checkCenterMove gameState cpileidx pcard@(Card pr _) 
    | iscpileempty && (pr == RK)        = False
    | iscpileempty && (pr /= RK)        = True
    | otherwise                         = isNextCard pcard (getCenterTop gameState cpileidx)
    where
        iscpileempty    = null (((gameState ^. field . center) !! cpileidx) ^. cards)

checkCornerMove :: GSt -> Int -> Card -> Bool
checkCornerMove gameState cpileidx pcard@(Card pr _)
    | iscpileempty && (pr == RK)        = True
    | otherwise                         = isNextCard pcard (getCornerTop gameState cpileidx)
    where
        iscpileempty    = null (((gameState ^. field . corner) !! cpileidx) ^. cards)

checkCen2CenMove :: GSt -> Int -> Int -> Bool
checkCen2CenMove gameState cpileidxf cpileidxt
    | isfcpileempty || istcpileempty = False
    | otherwise                      = isNextCard (getCenterBottom gameState cpileidxf) (getCenterTop gameState cpileidxt)
    where
        isfcpileempty   = null (((gameState ^. field . center) !! cpileidxf) ^. cards)
        istcpileempty   = null (((gameState ^. field . center) !! cpileidxt) ^. cards)

checkCen2CorMove :: GSt -> Int -> Int -> Bool
checkCen2CorMove gameState cpileidxf cpileidxt
    | isfcpileempty || istcpileempty = False
    | otherwise                      = isNextCard (getCenterBottom gameState cpileidxf) (getCornerTop gameState cpileidxt)
    where
        isfcpileempty   = null (((gameState ^. field . center) !! cpileidxf) ^. cards)
        istcpileempty   = null (((gameState ^. field . corner) !! cpileidxt) ^. cards)


-- check for different moves and their validity

canMove :: GSt -> Move -> Bool
canMove gameState move
    -- Corresponds to player drawing a card from draw pile
    | isfpiledraw && istpileplayer && hastpileidx && isdrawnotempty                 = 
        True
    -- Corresponds to player placing card on center pile
    | isfpileplayer && istpilecenter && iscardmove && hasfpileidx && hastpileidx    = 
        checkCenterMove gameState tpileidx (getPlayerCard gameState fpileidx fcardidx)
    -- Corresponds to player placing card on corner pile
    | isfpileplayer && istpilecorner && iscardmove && hasfpileidx && hastpileidx    = 
        checkCornerMove gameState tpileidx (getPlayerCard gameState fpileidx fcardidx)
    -- Corresponds to player placing one center pile on another center pile
    | isfpilecenter && istpilecenter && hasfpileidx && hastpileidx                  = 
        checkCen2CenMove gameState fpileidx tpileidx
    -- Corresponds to player placing one center pile on a corner pile
    | isfpilecenter && istpilecorner && hasfpileidx && hastpileidx                  = 
        checkCen2CorMove gameState fpileidx tpileidx
    -- All moves not explicitly checked are illegal
    | otherwise                                                                     = 
        False         
    where
        isfpiledraw     = move ^. fPileType == DrawP
        isfpileplayer   = move ^. fPileType == PlayerP
        isfpilecenter   = move ^. fPileType == CenterP
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

-- Function to compute score for a player hand

getScore :: [DCard] -> Int
getScore []             = 0
getScore (cd:cds)       
    | cr == RK          = 10 + getScore cds
    | otherwise         = 1 + getScore cds
    where
        Card cr _       = cd ^. card
