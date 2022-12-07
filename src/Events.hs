{-# LANGUAGE TemplateHaskell #-}

module Events
  ( handleEvent
  )
where

import Brick
import CardTypes
import GamePlay
import Graphics.Vty.Input (Event (..), Key (..))
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Utils
import Control.Monad.IO.Class
import Brick.Types (Next(..))

type GameState = GSt

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt
makeLenses ''PlayerInfo

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
handleEvent gs = case gs ^. screen of
  Welcome -> handleWelcome gs
  Game -> handleGame gs
  PopUp _ -> handlePopUp gs

handlePopUp :: GameState -> BrickEvent n e -> EventM n (Next GameState)
handlePopUp gs (VtyEvent (EvKey KEnter _)) = continue $ gs & screen .~ Game
handlePopUp gs _ = continue gs

handleWelcome :: GameState -> BrickEvent n e -> EventM n (Next GameState)
handleWelcome gs (VtyEvent (EvKey KEnter _)) = continue $ gs & screen .~ Game
handleWelcome gs _ = continue gs

handleGame :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleGame gs (VtyEvent (EvKey KRight _)) =
  continue $ incLook gs
handleGame gs (VtyEvent (EvKey KLeft _)) =
  continue $ decLook gs
-- Enter is our generic enter key. We are either making our first selection
-- or trying to make a move
handleGame gs (VtyEvent (EvKey KEnter _)) = case gs ^. selpileft of
  Nothing -> continue $ makeSelection gs -- there is no selection, make first selection
  Just _ -> if canMove newGS move
            then continue $ makeMove newGS move
            else continue $ resetMove gs -- creates popup
    where
      move = getMoveFromState newGS
      newGS = makeSecondSelection gs -- we have already made a selection, this one is our move

-- Up and Down move between decks
handleGame gs (VtyEvent (EvKey KUp _))
  | not $ haveSelection gs = case gs ^. looking of
      PlayerLook _ -> continue $ setLook (PileLook 0) gs
      _ -> continue gs
handleGame gs (VtyEvent (EvKey KDown _))
  | not $ haveSelection gs = case gs ^. looking of
      PileLook _ -> continue $ setLook (PlayerLook 0) gs
      _ -> continue gs
-- n goes to Next player, also handles AI case
handleGame gs (VtyEvent (EvKey (KChar 'n') _)) = case nextPlayerType of
  Human -> continue $ nextGS & screen .~ PopUp ("Next player: " ++ show nextPlayerIdx)
  AI -> suspendAndResume $ finalAIState gs
  where
    nextGS = nextPlayer gs
    nextPlayerType = ((nextGS ^. players) !! nextPlayerIdx) ^. ptype
    nextPlayerIdx = nextGS ^. toplay
    
    finalAIState :: GSt -> IO GSt
    finalAIState gameS = do
      nextMove <- getAIMove nextPlayerIdx gameS
      case nextMove of
        Just m -> finalAIState (makeMove gameS m)
        Nothing -> case nextPlayerGSType of
          AI -> finalAIState nextPlayerGS
          Human -> return (nextPlayerGS & screen .~ PopUp "AI Players have played.")
      where
        nextPlayerGS = nextPlayer gameS
        nextPlayerGSType = ((nextPlayerGS ^. players) !! (nextPlayerGS ^. toplay)) ^. ptype
        
-- 'q' toggles keyHelp
handleGame gs (VtyEvent (EvKey (KChar 'q') _)) =
  toggleHelp gs (gs ^. keyHelp)

handleGame s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleGame s _ = continue s

toggleHelp :: GameState -> (Int, Int) -> EventM n (Next GameState)
toggleHelp gs (0, 0)  = continue (gs & keyHelp .~ (80, 80))
toggleHelp gs (80, 80) = continue (gs & keyHelp .~ (0, 0))
