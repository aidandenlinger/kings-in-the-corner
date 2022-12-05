module Events
  ( handleEvent,
  )
where

import Brick
import CardTypes
import Graphics.Vty.Input (Event (..), Key (..))
import Utils

type GameState = GSt

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleEvent gs (VtyEvent (EvKey KRight _)) =
  continue $ incLook gs
handleEvent gs (VtyEvent (EvKey KLeft _)) =
  continue $ decLook gs
-- Up and Down move between decks
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KUp _)) =
--   continue (sel, (place + 1) `mod` 4, numCards)
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KDown _)) =
--   continue (sel, (place - 1) `mod` 4, numCards)
-- Esc quits game
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s
