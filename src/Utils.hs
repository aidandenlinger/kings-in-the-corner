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

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

------------------------------------------------------------------------------- 

-- Base code borrowed from https://github.com/ambuc/solitaire

-- assigns colors to suits

assignColor :: Suit -> Color
assignColor Spade   = Black
assignColor Club    = Black
assignColor _       = Red

-- Initialize game state for a new game

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- the default deal is a sorted list of cards. to be shuffled below
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]
