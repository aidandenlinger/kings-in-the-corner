module Utils
  ( assignColor
  ) where

import Data.List.Split (splitPlaces)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

-- makeLenses ''DCard
-- makeLenses ''Pile
-- makeLenses ''Field
-- makeLenses ''GSt

------------------------------------------------------------------------------- 

-- Base code borrowed from https://github.com/ambuc/solitaire

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



-- Initialize game state for a new game

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- the default deal is a sorted list of cards to be shuffled on game state initialization
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]
