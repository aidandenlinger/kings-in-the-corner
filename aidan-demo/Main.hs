module Main (main) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style as BS--(unicodeRounded, bsVertical)
import Brick.Widgets.Center
import Brick.Widgets.Core (cropBottomBy, updateAttrMap, withBorderStyle)
import Data.List (foldl1')
import Data.List.Index (modifyAt)
import Graphics.Vty (Event (..), Key (..))
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color (blue, green, red)
import System.Exit (exitSuccess)

-- width, height of cards
cardSize :: (Int, Int)
cardSize = (4, 3)

-- stolen from
-- <https://github.com/ambuc/solitaire/blob/0ada6e445c85f2f61c15081be49a99df6e272d29/src/Render.hs#L18>
-- given a widget, sets the border to be rounded :)
cardStyle :: Widget n -> Widget n
cardStyle = withBorderStyle BS.unicodeRounded . border

italicStyle :: Widget n -> Widget n
italicStyle = withBorderStyle custom . border

custom :: BS.BorderStyle
custom =
    BS.BorderStyle   { BS.bsCornerTL = '╒'
                  , BS.bsCornerTR = '╕'
                  , BS.bsCornerBR = '╛'
                  , BS.bsCornerBL = '╘'
                  , BS.bsHorizontal = '─'
                  , BS.bsVertical = '/'
                   }

-- takes in card text, centers it, makes it card size
cardWidget :: String -> Widget ()
cardWidget text = padTop Max $ padAll 1 $ italicStyle $ setAvailableSize cardSize $ center $ str text

cardWidgetNorthSouth :: String -> Widget ()
cardWidgetNorthSouth text = padLeftRight 30 $ cardStyle $ setAvailableSize cardSize $ center $ str text

cardWidgetNorthSouthBottom :: String -> Widget ()
cardWidgetNorthSouthBottom text = padLeftRight 30 $ cardStyle $ setAvailableSize (4, 2) $ center $ str text

cardWidgetEastWest :: String -> Widget ()
cardWidgetEastWest text = padLeftRight 13 $ cardStyle $ setAvailableSize cardSize $ center $ str text

cardWidgetEastWestBottom :: String -> Widget ()
cardWidgetEastWestBottom text = padLeftRight 13 $ cardStyle $ setAvailableSize (4, 2) $ center $ str text

-- attributes that widgets can use
attrs :: [(AttrName, Attr)]
attrs = [(attrName "selected_card", fg blue), (attrName "place_card", fg green)]

-- marks a card as selected
isSelected :: Widget n -> Widget n
isSelected = withAttr (attrName "selected_card")

-- where to place card
placeCard :: Widget n -> Widget n
placeCard = withAttr (attrName "place_card")

type GameState = (Int, Int, Int)

myAppHandleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
myAppHandleEvent (sel, place, numCards) (VtyEvent (EvKey KLeft _)) =
  continue ((sel - 1) `mod` numCards, place, numCards)
myAppHandleEvent (sel, place, numCards) (VtyEvent (EvKey KRight _)) =
  continue ((sel + 1) `mod` numCards, place, numCards)
myAppHandleEvent (sel, place, numCards) (VtyEvent (EvKey KUp _)) =
  continue (sel, (place + 1) `mod` 4, numCards)
myAppHandleEvent (sel, place, numCards) (VtyEvent (EvKey KDown _)) =
  continue (sel, (place - 1) `mod` 4, numCards)
myAppHandleEvent (sel, place, numCards) (VtyEvent (EvKey KEnter _)) =
  continue (sel, place, numCards)
myAppHandleEvent s (VtyEvent (EvKey KEsc [])) = halt s
myAppHandleEvent (sel, place, numCards) (MouseDown {}) =
  continue ((sel + 1) `mod` numCards, 0, numCards)
myAppHandleEvent s _ = continue s

playerHand :: [String]
playerHand = ["5❤️", "6♣︎", "7♥️"]

board :: [String]
board = ["8♠", "9♠", "6♣︎", "K♦️", "Q❤️", "J♦"]

-- start point of this executable
main :: IO ()
main = do
  let app =
        App
          { -- given a state, return list of widgets to draw. in this case,
            -- state is the index of the card selected and the number of cards.
            -- make the playerHand a series of widgets, make the selected card
            -- selected, then use a fold to combine them all horizontally, and
            -- finally vertically append some text that states what is selected
        appDraw = \(sel, place, _) ->
              [ ( (cropBottomBy 2 (cardWidgetNorthSouthBottom (board!!3))) <=> vBox (map (\x -> if (place == 0) then placeCard x else x) [cardWidgetNorthSouth (board!!0)])) <=>
                ((cropBottomBy 2 (cardWidgetEastWestBottom (board!!3))) <+> (cropBottomBy 2 (cardWidgetEastWestBottom (board!!3)))) <=>
                (hBox (map (\x -> if (place == 3) then placeCard x else x) [cardWidgetEastWest (board!!1)]) <+> hBox (map (\x -> if (place == 1) then placeCard x else x) [cardWidgetEastWest (board!!2)])) <=>
                ((cropBottomBy 2 (cardWidgetNorthSouthBottom (board!!5))) <=> vBox (map (\x -> if (place == 2) then placeCard x else x) [cardWidgetNorthSouth (board!!3)])) <=>
                (padLeftRight 20 (foldl1' (<+>) (modifyAt sel isSelected (map cardWidget playerHand))))
                  <=> str ("selected: " ++ show sel)
              ],
            -- given state and an event, describe how to change state. the app
            -- is then redrawn
            appHandleEvent = myAppHandleEvent,
            -- returns an EventM that runs at app start, this is a demo,
            -- there's nothing to do at start, return
            appStartEvent = return,
            -- holds attributes that can be applied to elements, ex
            -- selected_card
            appAttrMap = const $ attrMap defAttr attrs,
            -- don't show cursor
            appChooseCursor = neverShowCursor
          }
  -- use defaultMain to start our app
  -- use [] as our state since this app doesn't store anything
  -- ignore the final returned state, because this app doesn't store anything
  _ <- defaultMain app (0, 0, length playerHand)
  -- exit once done, don't check anything
  exitSuccess