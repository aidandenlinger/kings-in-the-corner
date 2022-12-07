module CardTypes
  ( Axis (..),
    Card (..),
    Color (..),
    DCard (..),
    DisplayMode (..),
    FaceDir (..),
    Field (..),
    Look (..),
    Screen (..),
    GSt (..),
    Pile (..),
    PileType (..),
    Rank (..),
    Suit (..),
    Move (..),
    PlayerInfo (..),
    PlayerType (..),
    WelcomeSelect (..)
  )
where

import qualified System.Random as R (StdGen)
import Test.QuickCheck

-- Base code borrowed from https://github.com/ambuc/solitaire

-- CARD TYPES ------------------------------------------------------------------
-- Define basic data types to handle cards

-- Data type defining order of cards

data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK
  deriving (Eq, Ord, Bounded, Enum)

-- Instantiate show with the card names
-- 10 is tricky so we manage it with a single character unicode

instance Show Rank where
  show RA = "A"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = [toEnum 0x2491] :: String -- unicode ligature for one-char width
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"

instance Arbitrary Rank where
  arbitrary = elements [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ]

-- Data type defining the suits

data Suit = Spade | Heart | Club | Diamond
  deriving (Eq, Ord, Bounded, Enum)

instance Arbitrary Suit where
  arbitrary = elements [Spade, Heart, Club, Diamond]

-- Instantiate show for suits with colored unicode symbols

instance Show Suit where
  show Spade = [toEnum 0x2660] :: String -- unicode characters for suits
  show Heart = [toEnum 0x2661] :: String
  show Diamond = [toEnum 0x25C7] :: String
  show Club = [toEnum 0x2663] :: String

-- Data type for Card. Contains 2 attributes rank and suit.

data Card = Card Rank Suit deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ show s

-- Data type for the direction card is facing in a pile

data FaceDir = FaceUp | FaceDown deriving (Eq, Show, Ord)

-- Data type for directional card. Contains a card and the facing direction

data DCard = DCard
  { _card :: Card,
    _facedir :: FaceDir
  }
  deriving (Eq, Show, Ord)

-- Data type for Display mode of cards

data DisplayMode = Stacked | Splayed | Sp3 deriving (Eq, Show, Ord)

-- Data type for different types of piles in the game
-- PlayerP  : The pile in hand of a player
-- DrawP    : The pile in the center from which the cards are drawn
-- CenterP  : One of the 4 piles along the straight lines from the center
-- CornerP  : One of the 4 piles in the corner headed by a King
-- NullP    : To handle no selected pile

data PileType = PlayerP | DrawP | CenterP | CornerP | NullP deriving (Eq, Show)

-- Data type for pile of cards.X

data Pile = Pile
  { _cards :: [DCard], --   List of cards in the pile in order
    _display :: DisplayMode, -- , opinions on how to be drawn
    _rankBias :: Maybe Rank, -- , Rank of top card
    _suitBias :: Maybe Suit, -- , Suit of top card
    _pileType :: PileType -- , Identified for top of pile
  }
  deriving (Eq, Show)

-- GAME TYPES ------------------------------------------------------------------
-- Data types required for gameplay

-- Data type for Player info

data PlayerType = Human | AI
  deriving (Eq, Ord, Bounded, Enum)

instance Show PlayerType where
  show Human = "Human"
  show AI = "AI"

data PlayerInfo = PInfo
  { _ptype :: PlayerType, -- The type of the player
    _difficulty :: Int -- Difficulty level for AI player. Larger is more difficult
  }
  deriving (Eq, Show)

-- Data type for the complete field

data Field = Field
  { _drawPile :: Pile,
    _centerPiles :: [Pile],
    _cornerPiles :: [Pile],
    _phands :: [Pile]
  }
  deriving (Eq, Show)

data Look = PlayerLook Int | PileLook Int deriving (Eq, Show)

-- Welcome fields: numPlayers numAI difficulty
data Screen = Welcome WelcomeSelect Int Int Int | Game | PopUp String deriving (Eq, Show)

data WelcomeSelect = NumPlayers | AIPlayers | Difficulty deriving (Eq, Show)

-- Gamestate data type recording game history and current play situation

data GSt = GSt
  { _field :: Field, -- Current state of decks
    _seed :: R.StdGen, -- A random seed to be passed thru
    _history :: [(Field, Int)], -- List of previous fields and corresponding player ids
    _toplay :: Int, -- Player id of the one with next move
    _looking :: Look, -- Where the player is currently looking
    _selpileft :: Maybe PileType, -- Pile type of the from pile if selected
    _selpilefi :: Maybe Int, -- Pile idx of the from pile if selected
    _selcdidx :: Maybe Int, -- Card idx from the from pile if selected
    _selpilett :: Maybe PileType, -- Pile type of the to pile if selected
    _selpileti :: Maybe Int, -- Pile idx of the to pile if selected
    _screen :: Screen, -- What screen we are displaying right now
    _keyHelp :: (Int, Int), -- KeyHelp
    _players :: [PlayerInfo] -- List of players playing
  }
  deriving (Show)

-- DISPLAY TYPES ---------------------------------------------------------------

-- Data types for displaying the cards

data Color = Red | Black deriving (Eq, Show, Ord)

data Axis = NS | EW deriving (Eq, Show) -- data type for pile splay orientation

-- Data type for capturing a suggested move

data Move = Move
  { _fPileType :: PileType, -- Type of pile to take the card / pile from
    _fPileIdx :: Maybe Int, -- Index of the pile in an array of piles. Nothing if fPileType is drawP
    _fCardIdx :: Maybe Int, -- Index of card in the pile to be placed. Applies only to player hands. Nothing otherwise
    _tPileType :: PileType, -- Type of pile to place the card / pile on
    _tPileIdx :: Maybe Int
  }
  deriving (Show)
