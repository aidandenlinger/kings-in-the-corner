{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( assignColor,
    initialDeal,
    genPlPile,
    initGSt,
    getPHands
  ) where

import Data.List.Split (splitPlaces)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

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

-- attempt to write/modify canPlace

canPlace :: Card -> Pile -> Bool -- says whether a card can be placed on a pile
-- given an empty Corner pile, a card needs to match both biases
canPlace (Card r  s ) Pile { _pileType = CornerP
                           , _cards    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
-- Not sure if we need suitBias in the above function. But this looks similar to the Ace foundation in solitare.

-- nonempty corner piles reject Kings (not needed?)
canPlace (Card RK _ ) Pile { _pileType = CornerP
                           , _cards    = (dc:_)
                           } = False

-- nonempty center piles accept cards if they alternate color and descend rank
canPlace (Card r  s ) Pile { _pileType = CenterP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (assignColor s /= assignColor s')

-- nonempty corner piles accept cards if they alternate color and descend rank
canPlace (Card r  s ) Pile { _pileType = CornerP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (assignColor s /= assignColor s')

-- center piles reject kings
canPlace (Card RK _ ) Pile { _pileType = CenterP
                           , _cards    = _
                           } = False

canPlace _ _ = False -- if not covered above, default invalid


canPlacePile :: Pile -> Pile -> Bool -- says whether a Pile can be placed on a pile
-- piles over another pile
canPlacePile Pile { _pileType = CenterP
                           , _rcards    = (DCard{_card=Card r s}:_)
                           , _rankBias = _
                           } Pile { _pileType = _
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = _
                           } = (succ r == r') && (assignColor s /= assignColor s')

canPlacePile _ _ = False -- if not covered above, default invalid

-- if a game is won, all 52 cards are in the foundation
hasWon :: GSt -> Bool
hasWon s = length (s ^. field . phands . traverse . cards) == 0

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

genCenCorPiles :: [Card] -> ([Pile], [Pile]) -> ([Pile], [Pile])
genCenCorPiles deal@((Card RK _):cs) (cenPiles, corPiles)
  | length cenPiles == 4  = (cenPiles, corPiles)
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
  | length cenPiles == 4  = (cenPiles, corPiles)
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

-- Adding to complete patter matching

genCenCorPiles [] (cenPiles, corPiles) = (cenPiles, corPiles)

-- take a random generator and initialize a game state

initGSt :: Int -> R.StdGen -> GSt
initGSt nPlayers seedval = GSt { _field   = fieldval,
                                 _seed    = seedval,
                                 _history = [],
                                 _toplay  = 0,
                                 _selcds  = Nothing,
                                 _selpile = Nothing
                               }
  where
    deal      = R.shuffle' initialDeal 52 seedval -- Shuffle the initial deal
    fieldval  = Field { _draw = drawval,
                        _center = centerval, 
                        _corner = cornerval,
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


getPHands :: GSt -> [Pile]
getPHands gameState = gameState ^. field . phands