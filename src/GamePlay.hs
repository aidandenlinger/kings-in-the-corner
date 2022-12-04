{-# LANGUAGE TemplateHaskell #-}

module GamePlay
  ( canMove,
    makeMove,
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

-- Function to compute score for a player hand

getScore :: [DCard] -> Int
getScore []             = 0
getScore (cd:cds)       
    | cr == RK          = 10 + getScore cds
    | otherwise         = 1 + getScore cds
    where
        Card cr _       = cd ^. card

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

-- Function to determine given a game state and an attempted move if it is possible
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

-- Helper functions to execute move

replaceInArr :: [a] -> Int -> a -> [a]
replaceInArr iArr idx updEl = take idx iArr ++ [updEl] ++ drop (idx + 1) iArr

removeFromArr :: [a] -> Int -> [a]
removeFromArr iArr idx = take idx iArr ++ drop (idx + 1) iArr

makeDrawMove :: GSt -> Int -> GSt
makeDrawMove iGameState pIdx = GSt { _field     = newfield,
                                     _seed      = iGameState ^. seed,
                                     _history   = newhistory,
                                     _toplay    = toplayidx,
                                     _selcdidx  = Nothing,
                                     _selpileft = Nothing,
                                     _selpilefi = Nothing,
                                     _selpilett = Nothing,
                                     _selpileti = Nothing
                                     }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _draw   = newdraw,
                              _center = iGameState ^. field . center, 
                              _corner = iGameState ^. field . corner,
                              _phands = newphands
                            }
        newdraw     = Pile { _cards     = drop 1 (iGameState ^. field . draw . cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = DrawP
                           }
        drawncard   = head (iGameState ^. field . draw . cards)
        updrawncard = drawncard & facedir .~ FaceUp
        newphand    = Pile { _cards     = updrawncard : (getPHands iGameState !! pIdx) ^. cards,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = DrawP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand

makeP2CenMove :: GSt -> Int -> Int -> Int -> GSt
makeP2CenMove iGameState pIdx cdIdx cIdx = GSt { _field     = newfield,
                                                 _seed      = iGameState ^. seed,
                                                 _history   = newhistory,
                                                 _toplay    = toplayidx,
                                                 _selcdidx  = Nothing,
                                                 _selpileft = Nothing,
                                                 _selpilefi = Nothing,
                                                 _selpilett = Nothing,
                                                 _selpileti = Nothing
                                                 }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _draw   = iGameState ^. field . draw,
                              _center = newcenter, 
                              _corner = iGameState ^. field . corner,
                              _phands = newphands
                            }
        newphand    = Pile { _cards     = removeFromArr ((getPHands iGameState !! pIdx) ^. cards) cdIdx,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = DrawP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand
        selcard     = ((getPHands iGameState !! pIdx) ^. cards) !! cdIdx
        newcpile    = Pile { _cards     = selcard : (((iGameState ^. field . center) !! cIdx) ^. cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CenterP
                           }
        newcenter   = replaceInArr (iGameState ^. field . center) cIdx newcpile

makeP2CorMove :: GSt -> Int -> Int -> Int -> GSt
makeP2CorMove iGameState pIdx cdIdx cIdx = GSt { _field     = newfield,
                                                 _seed      = iGameState ^. seed,
                                                 _history   = newhistory,
                                                 _toplay    = toplayidx,
                                                 _selcdidx  = Nothing,
                                                 _selpileft = Nothing,
                                                 _selpilefi = Nothing,
                                                 _selpilett = Nothing,
                                                 _selpileti = Nothing
                                                 }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _draw   = iGameState ^. field . draw,
                              _center = iGameState ^. field . center, 
                              _corner = newcorner,
                              _phands = newphands
                            }
        newphand    = Pile { _cards     = removeFromArr ((getPHands iGameState !! pIdx) ^. cards) cdIdx,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = DrawP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand
        selcard     = ((getPHands iGameState !! pIdx) ^. cards) !! cdIdx
        newcpile    = Pile { _cards     = selcard : (((iGameState ^. field . corner) !! cIdx) ^. cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CornerP
                           }
        newcorner   = replaceInArr (iGameState ^. field . corner) cIdx newcpile

makeCen2CenMove :: GSt -> Int -> Int -> GSt
makeCen2CenMove iGameState cIdxf cIdxt  = GSt { _field     = newfield,
                                                _seed      = iGameState ^. seed,
                                                _history   = newhistory,
                                                _toplay    = toplayidx,
                                                _selcdidx  = Nothing,
                                                _selpileft = Nothing,
                                                _selpilefi = Nothing,
                                                _selpilett = Nothing,
                                                _selpileti = Nothing
                                                }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _draw   = iGameState ^. field . draw,
                              _center = newcenter, 
                              _corner = iGameState ^. field . corner,
                              _phands = iGameState ^. field . phands
                            }
        fpile       = ((iGameState ^. field . center) !! cIdxf) ^. cards
        tpile       = ((iGameState ^. field . center) !! cIdxt) ^. cards
        newcpilef   = Pile { _cards     = [],
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CornerP
                           }
        newcpilet   = Pile { _cards     = fpile ++ tpile,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CornerP
                           }
        tempcenter  = replaceInArr (iGameState ^. field . center) cIdxf newcpilef
        newcenter   = replaceInArr tempcenter cIdxt newcpilet


-- Function to modify a game state given a valid move

makeMove :: GSt -> Move -> GSt
makeMove iGameState move 
    -- Corresponds to player drawing a card from draw pile
    | isfpiledraw && istpileplayer && hastpileidx && isdrawnotempty                 = 
        makeDrawMove iGameState tpileidx
    -- Corresponds to player placing card on center pile
    | isfpileplayer && istpilecenter && iscardmove && hasfpileidx && hastpileidx    = 
        makeP2CenMove iGameState fpileidx fcardidx tpileidx
    -- Corresponds to player placing card on corner pile
    | isfpileplayer && istpilecorner && iscardmove && hasfpileidx && hastpileidx    = 
        makeP2CorMove iGameState fpileidx fcardidx tpileidx
    -- Corresponds to player placing one center pile on another center pile
    | isfpilecenter && istpilecenter && hasfpileidx && hastpileidx                  = 
        makeCen2CenMove iGameState fpileidx tpileidx
    -- All other moves modify nothing
    | otherwise                                                                     = 
        iGameState
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
        isdrawnotempty  = not (null (iGameState ^. field . draw . cards))
