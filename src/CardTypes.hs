module CardTypes
  ( Axis(..)
  , Card(..)
  , Color(..)
  , DCard(..)
  , DisplayMode(..)
  , FaceDir(..)
  , Field(..)
  , GSt(..)
  , Pile(..)
  , PileType(..)
  , Rank(..) 
  , Suit(..)
  ) where

import qualified System.Random as R (StdGen)

-- Base code borrowed from https://github.com/ambuc/solitaire

-- CARD TYPES ------------------------------------------------------------------
-- Define basic data types to handle cards

-- Data type defining order of cards

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 
  deriving (Eq, Ord, Bounded, Enum)

-- Instantiate show with the card names
-- 10 is tricky so we manage it with a single character unicode

instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = [toEnum 0x2491] :: String; -- unicode ligature for one-char width 
  show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

-- Data type defining the suits

data Suit    = Spade | Heart | Club | Diamond 
  deriving (Eq, Ord, Bounded, Enum)

-- Instantiate show for suits with colored unicode symbols

instance Show Suit where
  show Spade   = [toEnum 0x2660] :: String -- unicode characters for suits
  show Heart   = [toEnum 0x2665] :: String
  show Diamond = [toEnum 0x2666] :: String 
  show Club    = [toEnum 0x2663] :: String

-- Data type for Card. Contains 2 attributes rank and suit.

data Card        = Card Rank Suit                    deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ show s

-- Data type for the direction card is facing in a pile

data FaceDir     = FaceUp | FaceDown                 deriving (Eq, Show, Ord)

-- Data type for directional card. Contains a card and the facing direction

data DCard       = DCard { _card    :: Card
                         , _facedir :: FaceDir }     deriving (Eq, Show, Ord)

-- Data type for Display mode of cards

data DisplayMode = Stacked | Splayed | Sp3           deriving (Eq, Show, Ord)

-- Data type for different types of piles in the game
-- PlayerP  : The pile in hand of a player
-- DrawP    : The pile in the center from which the cards are drawn
-- CenterP  : One of the 4 piles along the straight lines from the center
-- CornerP  : One of the 4 piles in the corner headed by a King

data PileType    = PlayerP | DrawP | CenterP | CornerP deriving (Eq, Show)

-- Data type for pile of cards.X

data Pile = Pile { _cards    :: [DCard]     --   List of cards in the pile in order
                 , _display  :: DisplayMode -- , opinions on how to be drawn
                 , _rankBias :: Maybe Rank  -- , Rank of top card
                 , _suitBias :: Maybe Suit  -- , Suit of top card
                 , _pileType :: PileType    -- , Identified for top of pile
                 } deriving (Eq, Show)

-- GAME TYPES ------------------------------------------------------------------
-- Data types required for gameplay

-- Data type for the complete field

data Field = Field { _draw    :: Pile
                   , _center  :: [Pile]
                   , _corner  :: [Pile]
                   , _phands  :: [Pile]
                   } deriving (Eq, Show)

-- Gamestate data type recording game history and current play situation

data GSt = GSt { _field   :: Field            -- Current state of decks
               , _seed    :: R.StdGen         -- A random seed to be passed thru
               , _history :: [(Field, Int)]   -- List of previous fields and corresponding player ids
               , _toplay  :: Int              -- Player id of the one with next move
               , _selcd   :: Maybe DCard      -- A card if selected in the graphics
               , _selpile :: Maybe Pile       -- A pile if selected in the graphics
               } deriving (Show)

-- DISPLAY TYPES ---------------------------------------------------------------

-- Data types for displaying the cards

data Color  = Red | Black                       deriving (Eq, Show, Ord)

data Axis   = NS | EW deriving (Eq, Show) -- data type for pile splay orientation

-- data Action = New | Undo deriving (Eq, Show, Ord) -- data type for button action

-- data Ext = StockX | WasteX | TableX | FoundX -- named extents for click regions
--          | IdX Int | DCX DCard | ActionX Action
--   deriving (Eq, Show, Ord)