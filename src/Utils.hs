{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( assignColor,
    initialDeal,
    genPlPile,
    initGSt,
    hasWon,
    getCurrP,
    getPHands,
    getCurrPCards,
    incLook,
    decLook,
    makeSelection,
    haveSelection,
    getPiles,
    updateToPlay,
    updateSelCardIdx,
    updateSelPileIdx,
    updateSelPileType,
    getMoveFromState
  ) where

import Data.Maybe (fromMaybe, isJust)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head, set )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes
import Data.List (transpose)

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

------------------------------------------------------------------------------- 

-- Base code borrowed from https://github.com/ambuc/solitaire

-- Initialize any constants that might be needed

nCardsInHand :: Int
nCardsInHand = 7

-- assigns colors to suits

assignColor :: Suit -> Color
assignColor Spade   = Black
assignColor Club    = Black
assignColor _       = Red

-- Function to check if the player with the given index has won
-- if a game is won, a player hand pile cards has length 0

hasWon :: GSt -> Int -> Bool
hasWon s idx = null ((getPHands s !! idx) ^. cards)

-- A set of get and update functions to allow for game state read and modification

-- Get the index of current player

getCurrP :: GSt -> Int
getCurrP gameState = gameState ^. toplay

-- Get all player hands in the current game state

getPHands :: GSt -> [Pile]
getPHands gameState = gameState ^. field . phands

pileToCards :: Pile -> [Card]
pileToCards p = map (^. card) (p ^. cards)

-- Get player cards for current player
getCurrPCards :: GSt -> [Card]
getCurrPCards gameState = pileToCards (getPHands gameState !! getCurrP gameState)

-- Look functions
updateLook :: (Int -> Int) -> GSt -> GSt
updateLook func gameState = case gameState ^. looking of
  PlayerLook idx -> flip setLook gameState $
                    PlayerLook (func idx `mod` length (getCurrPCards gameState))
  PileLook idx -> flip setLook gameState $
                  PileLook (func idx `mod` length (getPiles gameState))

setLook :: Look -> GSt -> GSt
setLook l gs = gs & looking .~ l

incLook :: GSt -> GSt
incLook = updateLook (+ 1)

decLook :: GSt -> GSt
decLook = updateLook (\x -> x - 1)

makeSelection :: GSt -> GSt
makeSelection gs = case gs ^. selpileft of
  Nothing -> case gs ^. looking of
    -- move Look to GameState selection, start looking at piles
    PlayerLook idx -> setLook (PileLook 0) $
                      updateSelPileIdx True (Just $ getCurrP gs) $
                      updateSelCardIdx (Just idx) $
                      updateSelPileType True (Just PlayerP) gs
    PileLook _pileidx -> undefined
  Just _ -> gs -- TODO: allow undoing selection

haveSelection :: GSt -> Bool
haveSelection gs = isJust (gs ^. selpileft)

-- Gets the center and corner piles for graphics. DOES NOT GET DRAW PILE (since we don't want
-- to display what the draw card is!)
-- <https://stackoverflow.com/a/22702925>
getPiles :: GSt -> [[Card]]
getPiles gs = map pileToCards [topleft, top, topright, left, draw, right, bottomleft, bottom, bottomright]
  where
    draw = gs ^. field . drawPile
    [topleft, topright, bottomleft, bottomright] = gs ^. field . cornerPiles
    [top, left, right, bottom] = gs ^. field . centerPiles

-- Update the index of the player currently to play

updateToPlay :: Int -> GSt -> GSt
updateToPlay pId stState = stState & toplay .~ pId

-- Update the selected card to a different value

updateSelCardIdx :: Maybe Int -> GSt -> GSt
updateSelCardIdx sCardIdx stState = stState & selcdidx .~ sCardIdx

-- Update the selected pile to a different value
-- True corresponds to updating the from pile
-- False corresponds to updating the to pile

updateSelPileType :: Bool -> Maybe PileType -> GSt -> GSt
updateSelPileType True sPileType stState = stState & selpileft .~ sPileType
updateSelPileType False sPileType stState = stState & selpilett .~ sPileType

updateSelPileIdx :: Bool -> Maybe Int -> GSt -> GSt
updateSelPileIdx True sPileIdx stState = stState & selpilefi .~ sPileIdx
updateSelPileIdx False sPileIdx stState = stState & selpileti .~ sPileIdx

-- Function to generate a move object from a given game state

getMoveFromState :: GSt -> Move
getMoveFromState gameState = Move { _fPileType  = fpilety,
                                    _fPileIdx   = gameState ^. selpilefi,
                                    _fCardIdx   = gameState ^. selcdidx,
                                    _tPileType  = tpilety,
                                    _tPileIdx   = gameState ^. selpileti
                                    }
  where
    fpilety = fromMaybe NullP (gameState ^. selpileft)
    tpilety = fromMaybe NullP (gameState ^. selpilett)

-- Initialize game state for a new game

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- the default deal is a sorted list of cards to be shuffled on game state initialization
initialDeal :: [Card]
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

-- Generate a pile for n players from the given deal of cards
-- This is done by taking the top 7 cards and recursively calling the function
-- Assume there are enough cards in the deck

genPlPile :: Int -> [Card] -> [Pile]
genPlPile nPlayers deal 
  | nPlayers <= 0 = []
  | otherwise     = initHand : genPlPile (nPlayers - 1) (drop nCardsInHand deal)
  where
    initHand      = Pile { _cards    = [ DCard { _card    = c,
                                                  _facedir = FaceDown 
                                                } 
                                          | c <- take nCardsInHand deal
                                        ],
                           _display  = Stacked,
                           _rankBias = Nothing,
                           _suitBias = Nothing,
                           _pileType = PlayerP
                         }     

-- Generate center and corner piles for initialization

-- Ensure we have 4 corner piles

padCorPiles :: ([Pile], [Pile]) -> ([Pile], [Pile])
padCorPiles (cenPiles, corPiles)
  | length corPiles == 4  = (cenPiles, corPiles)
  | otherwise             = padCorPiles (cenPiles, initCorPile:corPiles)
  where
    initCorPile     = Pile { _cards    = [],
                             _display  = Stacked,
                             _rankBias = Nothing,
                             _suitBias = Nothing,
                             _pileType = CornerP
                           }


genCenCorPiles :: [Card] -> ([Pile], [Pile]) -> ([Pile], [Pile])
genCenCorPiles deal@((Card RK _):cs) (cenPiles, corPiles)
  | length cenPiles == 4  = padCorPiles (cenPiles, corPiles)
  | otherwise             = genCenCorPiles cs (cenPiles, initCorPile:corPiles)
  where
    initCorPile     = Pile { _cards    = [ DCard { _card    = ctop,
                                                  _facedir = FaceUp 
                                                } 
                                          | ctop <- take 1 deal
                                        ],
                             _display  = Stacked,
                             _rankBias = Nothing,
                             _suitBias = Nothing,
                             _pileType = CornerP
                           }

genCenCorPiles (c:cs) (cenPiles, corPiles)
  | length cenPiles == 4  = padCorPiles (cenPiles, corPiles)
  | otherwise             = genCenCorPiles cs (initCenPile:cenPiles, corPiles)
  where
    initCenPile     = Pile { _cards    = [ DCard { _card    = c,
                                                  _facedir = FaceUp 
                                                } 
                                        ],
                             _display  = Stacked,
                             _rankBias = Nothing,
                             _suitBias = Nothing,
                             _pileType = CenterP
                           }

-- Adding to complete pattern matching

genCenCorPiles [] (cenPiles, corPiles) = (cenPiles, corPiles)

-- take a random generator and initialize a game state

initGSt :: Int -> R.StdGen -> GSt
initGSt nPlayers seedval = GSt { _field     = fieldval,
                                 _seed      = seedval,
                                 _history   = [],
                                 _toplay    = 0,
                                 _looking   = PlayerLook 0,
                                 _selcdidx  = Nothing,
                                 _selpileft = Nothing,
                                 _selpilefi = Nothing,
                                 _selpilett = Nothing,
                                 _selpileti = Nothing
                               }
  where
    deal      = R.shuffle' initialDeal 52 seedval -- Shuffle the initial deal
    fieldval  = Field { _drawPile = drawval,
                        _centerPiles = centerval, 
                        _cornerPiles = cornerval,
                        _phands = phandsval
                      }
    phandsval = genPlPile nPlayers deal
    centerval = fst ccpiles
    cornerval = snd ccpiles
    ccpiles   = genCenCorPiles (drop (nPlayers * nCardsInHand) deal) ([], [])
    drawval   = Pile { _cards    = [ DCard { _card    = c,
                                            _facedir = FaceUp 
                                          } 
                                    | c <- drop ((nPlayers * nCardsInHand) + length centerval + length cornerval) deal
                                  ],
                       _display  = Stacked,
                       _rankBias = Nothing,
                       _suitBias = Nothing,
                       _pileType = DrawP
                     }
