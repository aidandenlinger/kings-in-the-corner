{-# LANGUAGE TemplateHaskell #-}

module Events
  ( handleEvent,
  )
where

import Brick
import CardTypes
import GamePlay
import Graphics.Vty.Input (Event (..), Key (..))
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Utils

type GameState = GSt

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleEvent gs (VtyEvent (EvKey KRight _)) =
  continue $ incLook gs
handleEvent gs (VtyEvent (EvKey KLeft _)) =
  continue $ decLook gs
-- Enter is our generic enter key. We are either making our first selection
-- or trying to make a move
handleEvent gs (VtyEvent (EvKey KEnter _)) = case gs ^. selpileft of
  Nothing -> continue $ makeSelection gs -- there is no selection, make first selection
  -- TODO: error popup when move is invalid to let user know
  Just _ -> if canMove newGS move then continue $ makeMove newGS move else continue $ resetMove gs
    where
      move = getMoveFromState newGS
      newGS = makeSecondSelection gs -- we have already made a selection, this one is our move

-- Up and Down move between decks
handleEvent gs (VtyEvent (EvKey KUp _))
  | not $ haveSelection gs = case gs ^. looking of
      PlayerLook _ -> continue $ setLook (PileLook 0) gs
      _ -> continue gs
handleEvent gs (VtyEvent (EvKey KDown _))
  | not $ haveSelection gs = case gs ^. looking of
      PileLook _ -> continue $ setLook (PlayerLook 0) gs
      _ -> continue gs
-- n goes to Next player
handleEvent gs (VtyEvent (EvKey (KChar 'n') _)) = continue $ nextPlayer gs

--make welcome screen disappear
handleEvent gs (VtyEvent (EvKey (KChar ' ') _)) =
  continue (gs & welcome .~ (90, 90))

-- 'q' toggles keyHelp
handleEvent gs (VtyEvent (EvKey (KChar 'q') _)) =
  toggleHelp gs (gs ^. keyHelp)

handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s

toggleHelp :: GameState -> (Int, Int) -> EventM n (Next GameState)
toggleHelp gs (0, 0)  = continue (gs & keyHelp .~ (80, 80))
toggleHelp gs (80, 80) = continue (gs & keyHelp .~ (0, 0))
