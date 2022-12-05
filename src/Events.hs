{-# LANGUAGE TemplateHaskell #-}

module Events
  ( handleEvent,
  )
where

import Brick
import CardTypes
import Graphics.Vty.Input (Event (..), Key (..))
import Lens.Micro.TH (makeLenses)
import Utils
import Lens.Micro

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
  Nothing -> continue $ lookToFromSelection gs -- there is no selection, make first selection
  Just _ -> undefined -- we have already made a selection, this one is our move

-- Up and Down move between decks
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KUp _)) =
--   continue (sel, (place + 1) `mod` 4, numCards)
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KDown _)) =
--   continue (sel, (place - 1) `mod` 4, numCards)
-- Esc quits game
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s
