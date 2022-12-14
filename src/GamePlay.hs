{-# LANGUAGE TemplateHaskell #-}

module GamePlay
  ( canMove,
    makeMove,
    getScore,
    resetMove,
    nextPlayer,
    winPopUp,
    isNextCard,
    getAIMove,
    getCardMoves,
    getPileMoves,
    selFromMoves,
    initGStP1Draw
  ) where

import Data.Maybe (isJust, fromMaybe)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head, set )
import Lens.Micro.TH (makeLenses)
import Test.QuickCheck
import Prelude

import CardTypes
import Utils
import qualified System.Random as R

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt
makeLenses ''Move
makeLenses ''PlayerInfo

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
getCenterTop gameState pileidx = ((((gameState ^. field . centerPiles) !! pileidx) ^. cards) !! topOfPileIdx) ^. card

getCenterBottom :: GSt -> Int -> Card
getCenterBottom gameState pileidx = last (((gameState ^. field . centerPiles) !! pileidx) ^. cards) ^. card

getCornerTop :: GSt -> Int -> Card
getCornerTop gameState pileidx = ((((gameState ^. field . cornerPiles) !! pileidx) ^. cards) !! topOfPileIdx) ^. card

-- Helper functions to check for move validity

isNextCard :: Card -> Card -> Bool
isNextCard (Card RK _) _                = False
isNextCard (Card rf sf) (Card rt st)    = (succ rf == rt) && (assignColor sf /= assignColor st)

checkCenterMove :: GSt -> Int -> Card -> Bool
checkCenterMove gameState cpileidx pcard@(Card pr _) 
    | iscpileempty && (pr == RK)        = False
    | iscpileempty && (pr /= RK)        = True
    | otherwise                         = isNextCard pcard (getCenterTop gameState cpileidx)
    where
        iscpileempty    = null (((gameState ^. field . centerPiles) !! cpileidx) ^. cards)

checkCornerMove :: GSt -> Int -> Card -> Bool
checkCornerMove gameState cpileidx pcard@(Card pr _)
    | iscpileempty && (pr == RK)        = True
    | iscpileempty && (pr /= RK)        = False
    | otherwise                         = isNextCard pcard (getCornerTop gameState cpileidx)
    where
        iscpileempty    = null (((gameState ^. field . cornerPiles) !! cpileidx) ^. cards)

checkCen2CenMove :: GSt -> Int -> Int -> Bool
checkCen2CenMove gameState cpileidxf cpileidxt
    | isfcpileempty || istcpileempty    = False
    | cpileidxf == cpileidxt            = False
    | otherwise                         = isNextCard (getCenterBottom gameState cpileidxf) (getCenterTop gameState cpileidxt)
    where
        isfcpileempty   = null (((gameState ^. field . centerPiles) !! cpileidxf) ^. cards)
        istcpileempty   = null (((gameState ^. field . centerPiles) !! cpileidxt) ^. cards)

checkCen2CorMove :: GSt -> Int -> Int -> Bool
checkCen2CorMove gameState cpileidxf cpileidxt
    | isfcpileempty || istcpileempty = False
    | otherwise                      = isNextCard (getCenterBottom gameState cpileidxf) (getCornerTop gameState cpileidxt)
    where
        isfcpileempty   = null (((gameState ^. field . centerPiles) !! cpileidxf) ^. cards)
        istcpileempty   = null (((gameState ^. field . cornerPiles) !! cpileidxt) ^. cards)

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
        isdrawnotempty  = not (null (gameState ^. field . drawPile . cards))

-- init a game state and make a draw for P1
initGStP1Draw :: Int -> Int -> R.StdGen -> GSt
initGStP1Draw nPlayers nAI seedval = newAfterP1Draw
  where
    newAfterP1Draw = makeMove newForDraw (getMoveFromState newForDraw)
    newForDraw =  updateSelPileType True (Just DrawP) $
                  updateSelPileType False (Just PlayerP) $
                  updateSelPileIdx False (Just 0) new
    new = initGSt nPlayers nAI seedval

-- Updates the toplay index and draws a card for the next player.
nextPlayer :: GSt -> GSt
nextPlayer gs = updateToPlay nextP gsAfterDraw
  where
    nextP = (getCurrP gs + 1) `mod` length (getPHands gs) 
    gsForDraw = updateSelPileType True (Just DrawP) $
                updateSelPileType False (Just PlayerP) $
                updateSelPileIdx False (Just nextP) gs
    move = getMoveFromState gsForDraw
    gsAfterDraw = if canMove gsForDraw move
                    then makeMove gsForDraw (getMoveFromState gsForDraw)
                    else resetMove gsForDraw & screen .~ Game -- no popup

winPopUp :: GSt -> GSt
winPopUp gs = gs & screen .~ PopUp winmsg
    where
        winmsg = "Game Over!\nLow points is better! Esc to quit\n" ++ unlines (zipWith (\p c -> "Player " ++ show p ++ " (" ++ show (((gs ^. players) !! (p - 1)) ^. ptype) ++ ") : " ++ show (getScore c) ++ " points") [1..] (map (^. cards) (getPHands gs)))

-- Helper functions to execute move

replaceInArr :: [a] -> Int -> a -> [a]
replaceInArr iArr idx updEl = take idx iArr ++ [updEl] ++ drop (idx + 1) iArr

removeFromArr :: [a] -> Int -> [a]
removeFromArr iArr idx = take idx iArr ++ drop (idx + 1) iArr

-- Update game state for a draw move

makeDrawMove :: GSt -> Int -> GSt
makeDrawMove iGameState pIdx = GSt { _field     = newfield,
                                     _seed      = iGameState ^. seed,
                                     _history   = newhistory,
                                     _toplay    = toplayidx,
                                     _looking   = PlayerLook 0,
                                     _selcdidx  = Nothing,
                                     _selpileft = Nothing,
                                     _selpilefi = Nothing,
                                     _selpilett = Nothing,
                                     _selpileti = Nothing,
                                     _screen   = iGameState ^. screen,
                                     _keyHelp = iGameState ^. keyHelp,
                                     _players = iGameState ^. players
                                     }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _drawPile   = newdraw,
                              _centerPiles = iGameState ^. field . centerPiles, 
                              _cornerPiles = iGameState ^. field . cornerPiles,
                              _phands = newphands
                            }
        newdraw     = Pile { _cards     = drop 1 (iGameState ^. field . drawPile . cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = DrawP
                           }
        drawncard   = head (iGameState ^. field . drawPile . cards)
        updrawncard = drawncard & facedir .~ FaceUp
        newphand    = Pile { _cards     = updrawncard : (getPHands iGameState !! pIdx) ^. cards,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = PlayerP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand

-- Update game state to remove failed move
resetMove :: GSt -> GSt
resetMove gs = GSt { _field = gs ^. field,
                     _seed = gs ^. seed,
                     _history = gs ^. history,
                     _toplay = gs ^. toplay,
                     _looking = PlayerLook 0,
                     _selcdidx  = Nothing,
                     _selpileft = Nothing,
                     _selpilefi = Nothing,
                     _selpilett = Nothing,
                     _selpileti = Nothing,
                     _screen   = PopUp "Invalid move!",
                     _keyHelp = gs ^. keyHelp,
                     _players = gs ^. players
                   }

-- Update game state for a card from player hand to center pile

makeP2CenMove :: GSt -> Int -> Int -> Int -> GSt
makeP2CenMove iGameState pIdx cdIdx cIdx = GSt { _field     = newfield,
                                                 _seed      = iGameState ^. seed,
                                                 _history   = newhistory,
                                                 _toplay    = toplayidx,
                                                 _looking   = PlayerLook 0,
                                                 _selcdidx  = Nothing,
                                                 _selpileft = Nothing,
                                                 _selpilefi = Nothing,
                                                 _selpilett = Nothing,
                                                 _selpileti = Nothing,
                                                 _screen   = iGameState ^. screen,
                                                 _keyHelp = iGameState ^. keyHelp,
                                                 _players = iGameState ^. players
                                                 }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _drawPile   = iGameState ^. field . drawPile,
                              _centerPiles = newcenter, 
                              _cornerPiles = iGameState ^. field . cornerPiles,
                              _phands = newphands
                            }
        newphand    = Pile { _cards     = removeFromArr ((getPHands iGameState !! pIdx) ^. cards) cdIdx,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = PlayerP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand
        selcard     = ((getPHands iGameState !! pIdx) ^. cards) !! cdIdx
        newcpile    = Pile { _cards     = selcard : (((iGameState ^. field . centerPiles) !! cIdx) ^. cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CenterP
                           }
        newcenter   = replaceInArr (iGameState ^. field . centerPiles) cIdx newcpile

-- Update game state for a card from player hand to corner pile

makeP2CorMove :: GSt -> Int -> Int -> Int -> GSt
makeP2CorMove iGameState pIdx cdIdx cIdx = GSt { _field     = newfield,
                                                 _seed      = iGameState ^. seed,
                                                 _history   = newhistory,
                                                 _toplay    = toplayidx,
                                                 _looking   = PlayerLook 0,
                                                 _selcdidx  = Nothing,
                                                 _selpileft = Nothing,
                                                 _selpilefi = Nothing,
                                                 _selpilett = Nothing,
                                                 _selpileti = Nothing,
                                                 _screen  = iGameState ^. screen,
                                                 _keyHelp = iGameState ^. keyHelp,
                                                 _players = iGameState ^. players
                                                 }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _drawPile   = iGameState ^. field . drawPile,
                              _centerPiles = iGameState ^. field . centerPiles, 
                              _cornerPiles = newcorner,
                              _phands = newphands
                            }
        newphand    = Pile { _cards     = removeFromArr ((getPHands iGameState !! pIdx) ^. cards) cdIdx,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = PlayerP
                           }
        newphands   = replaceInArr (getPHands iGameState) pIdx newphand
        selcard     = ((getPHands iGameState !! pIdx) ^. cards) !! cdIdx
        newcpile    = Pile { _cards     = selcard : (((iGameState ^. field . cornerPiles) !! cIdx) ^. cards),
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CornerP
                           }
        newcorner   = replaceInArr (iGameState ^. field . cornerPiles) cIdx newcpile

-- Update game state for a center pile to another center pile

makeCen2CenMove :: GSt -> Int -> Int -> GSt
makeCen2CenMove iGameState cIdxf cIdxt  = GSt { _field     = newfield,
                                                _seed      = iGameState ^. seed,
                                                _history   = newhistory,
                                                _toplay    = toplayidx,
                                                _looking   = PlayerLook 0,
                                                _selcdidx  = Nothing,
                                                _selpileft = Nothing,
                                                _selpilefi = Nothing,
                                                _selpilett = Nothing,
                                                _selpileti = Nothing,
                                                _screen  = iGameState ^. screen,
                                                _keyHelp = iGameState ^. keyHelp,
                                                _players = iGameState ^. players
                                                }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _drawPile   = iGameState ^. field . drawPile,
                              _centerPiles = newcenter, 
                              _cornerPiles = iGameState ^. field . cornerPiles,
                              _phands = iGameState ^. field . phands
                            }
        fpile       = ((iGameState ^. field . centerPiles) !! cIdxf) ^. cards
        tpile       = ((iGameState ^. field . centerPiles) !! cIdxt) ^. cards
        newcpilef   = Pile { _cards     = [],
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CenterP
                           }
        newcpilet   = Pile { _cards     = fpile ++ tpile,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CenterP
                           }
        tempcenter  = replaceInArr (iGameState ^. field . centerPiles) cIdxf newcpilef
        newcenter   = replaceInArr tempcenter cIdxt newcpilet

-- Update game state for a center pile to a corner pile

makeCen2CorMove :: GSt -> Int -> Int -> GSt
makeCen2CorMove iGameState cIdxf cIdxt  = GSt { _field     = newfield,
                                                _seed      = iGameState ^. seed,
                                                _history   = newhistory,
                                                _toplay    = toplayidx,
                                                _looking   = PlayerLook 0,
                                                _selcdidx  = Nothing,
                                                _selpileft = Nothing,
                                                _selpilefi = Nothing,
                                                _selpilett = Nothing,
                                                _selpileti = Nothing,
                                                _screen   = iGameState ^. screen,
                                                _keyHelp = iGameState ^. keyHelp,
                                                _players = iGameState ^. players
                                                }
    where
        toplayidx   = iGameState ^. toplay
        newhistory  = (iGameState ^. field, toplayidx):(iGameState ^. history)
        newfield    = Field { _drawPile   = iGameState ^. field . drawPile,
                              _centerPiles = newcenter, 
                              _cornerPiles = newcorner,
                              _phands = iGameState ^. field . phands
                            }
        fpile       = ((iGameState ^. field . centerPiles) !! cIdxf) ^. cards
        tpile       = ((iGameState ^. field . cornerPiles) !! cIdxt) ^. cards
        newcpilef   = Pile { _cards     = [],
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CenterP
                           }
        newcpilet   = Pile { _cards     = fpile ++ tpile,
                             _display   = Stacked,
                             _rankBias  = Nothing,
                             _suitBias  = Nothing,
                             _pileType  = CornerP
                           }
        newcenter   = replaceInArr (iGameState ^. field . centerPiles) cIdxf newcpilef
        newcorner   = replaceInArr (iGameState ^. field . cornerPiles) cIdxt newcpilet


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
    -- Corresponds to player placing one center pile on a corner pile
    | isfpilecenter && istpilecorner && hasfpileidx && hastpileidx                  = 
        makeCen2CorMove iGameState fpileidx tpileidx
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
        isdrawnotempty  = not (null (iGameState ^. field . drawPile . cards))


-- Goes through possible moves and returns a move for the Ai to make

-- Get all possible card to center pile moves for the player

getCardCorMoves :: Int -> Int -> Int -> GSt -> [Move]
getCardCorMoves pIdx cardIdx cPileIdx gameState
    | cPileIdx < numCPiles  = if canMove gameState candMove then candMove:movest else movest
    | otherwise             = []
    where
        numCPiles   = length (gameState ^. field . cornerPiles)
        candMove    = Move { _fPileType = PlayerP,
                             _fPileIdx  = Just pIdx,
                             _fCardIdx  = Just cardIdx,
                             _tPileType = CornerP, 
                             _tPileIdx  = Just cPileIdx
                             }
        movest      = getCardCorMoves pIdx cardIdx (cPileIdx+1) gameState

-- Get all possible card to corner pile moves for the player

getCardCenMoves :: Int -> Int -> Int -> GSt -> [Move]
getCardCenMoves pIdx cardIdx cPileIdx gameState
    | cPileIdx < numCPiles  = if canMove gameState candMove then candMove:movest else movest
    | otherwise             = []
    where
        numCPiles   = length (gameState ^. field . centerPiles)
        candMove    = Move { _fPileType = PlayerP,
                             _fPileIdx  = Just pIdx,
                             _fCardIdx  = Just cardIdx,
                             _tPileType = CenterP, 
                             _tPileIdx  = Just cPileIdx
                             }
        movest      = getCardCenMoves pIdx cardIdx (cPileIdx+1) gameState

-- Get all possible card moves for the player

getCardMoves :: Int -> Int -> GSt -> [Move]
getCardMoves pIdx cardIdx gameState
    | cardIdx < numCardsinHand = 
        getCardCenMoves pIdx cardIdx 0 gameState ++ getCardCorMoves pIdx cardIdx 0 gameState ++ getCardMoves pIdx (cardIdx + 1) gameState
    | otherwise                = 
        []
    where
        numCardsinHand = length ((getPHands gameState !! pIdx) ^. cards)


-- Get center to corner pile moves available

getCenCorMoves :: Int -> Int -> GSt -> [Move]
getCenCorMoves cPileIdxf cPileIdxt gameState
    | cPileIdxt < numCPiles     = if canMove gameState candMove then candMove:movest else movest
    | otherwise                 = []
    where
        numCPiles   = length (gameState ^. field . cornerPiles)
        candMove    = Move { _fPileType = CenterP,
                             _fPileIdx  = Just cPileIdxf,
                             _fCardIdx  = Nothing,
                             _tPileType = CornerP, 
                             _tPileIdx  = Just cPileIdxt
                             }
        movest      = getCenCorMoves cPileIdxf (cPileIdxt + 1) gameState


-- Get center to center pile moves available

getCenCenMoves :: Int -> Int -> GSt -> [Move]
getCenCenMoves cPileIdxf cPileIdxt gameState
    | cPileIdxt < numCPiles     = if canMove gameState candMove then candMove:movest else movest
    | otherwise                 = []
    where
        numCPiles   = length (gameState ^. field . centerPiles)
        candMove    = Move { _fPileType = CenterP,
                             _fPileIdx  = Just cPileIdxf,
                             _fCardIdx  = Nothing,
                             _tPileType = CenterP, 
                             _tPileIdx  = Just cPileIdxt
                             }
        movest      = getCenCenMoves cPileIdxf (cPileIdxt + 1) gameState

-- Get all possible pile moves available

getPileMoves :: GSt -> Int -> [Move]
getPileMoves gameState cenPileIdx
    | cenPileIdx < numCPiles  = 
        getCenCenMoves cenPileIdx 0 gameState ++ getCenCorMoves cenPileIdx 0 gameState ++ getPileMoves gameState (cenPileIdx + 1)
    | otherwise             =
        []
    where
        numCPiles   = length (gameState ^. field . centerPiles)

-- Assumes the player being passed in is an AI

getAIMove :: Int -> GSt -> IO (Maybe Move)
getAIMove pIdx gameState = generate (selFromMoves cardMoves pileMoves dlev)
    where
        cardMoves   = getCardMoves pIdx 0 gameState
        pileMoves   = getPileMoves gameState 0
        dlev        = ((gameState ^. players) !! pIdx) ^. difficulty

selFromMoves :: [Move] -> [Move] -> Int -> Gen (Maybe Move)
selFromMoves cdMoves piMoves dlev
    | not (null cdMoves) && not (null piMoves)  = 
        frequency[((6 * dlev) + 3, elements cdMovesJ), ((4 * dlev) + 3, elements piMovesJ), (max (10 - (2 * dlev)) 1, elements [Nothing])]
    | not (null cdMoves) && null piMoves        = 
        frequency[((6 * dlev) + 3, elements cdMovesJ), (max (5 - (2 * dlev)) 1, elements [Nothing])]
    | null cdMoves && not (null piMoves)        = 
        frequency[((4 * dlev) + 3, elements piMovesJ), (max (5 - (2 * dlev)) 1, elements [Nothing])]
    | otherwise                                 =
        elements [Nothing]
    where
        cdMovesJ    = map (\m -> Just m) cdMoves
        piMovesJ    = map (\m -> Just m) piMoves