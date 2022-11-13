module Main (main) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color (blue)
import System.Exit (exitSuccess)
import Data.List (foldl1')

-- width, height of cards
cardSize :: (Int, Int)
cardSize = (4, 3)

-- stolen from
-- <https://github.com/ambuc/solitaire/blob/0ada6e445c85f2f61c15081be49a99df6e272d29/src/Render.hs#L18>
-- given a widget, sets the border to be rounded :)
cardStyle :: Widget n -> Widget n
cardStyle = withBorderStyle unicodeRounded . border

-- takes in card text, centers it, makes it card size
cardWidget :: String -> Widget ()
cardWidget text = cardStyle $ setAvailableSize cardSize $ center $ str text

-- attributes that widgets can use
attrs :: [(AttrName, Attr)]
attrs = [(attrName "selected_card", fg blue)]

-- marks a card as selected
isSelected :: Widget n -> Widget n
isSelected = withAttr (attrName "selected_card")

-- start point of this executable
main :: IO ()
main = do
  let app =
        App
          { -- given a state, return list of widgets to draw. in this case,
            -- ignore state and just draw card, with label as selected
            appDraw = const [foldl1' (<+>) $ map cardWidget ["5❤️", "6❤️", "7❤️"]],
            -- given state and an event, describe how to change state. this
            -- helper func in Brick.Main halts the program on any event
            -- except a resize
            appHandleEvent = resizeOrQuit,
            -- returns an EventM that runs at app start, this is a demo,
            -- there's nothing to do at start, return
            appStartEvent = return,
            -- holds attributes that can be applied to elements
            appAttrMap = const $ attrMap defAttr attrs,
            -- don't show cursor
            appChooseCursor = neverShowCursor
          }
  -- use defaultMain to start our app
  -- use [] as our state since this app doesn't store anything
  -- ignore the final returned state, because this app doesn't store anything
  _ <- defaultMain app (0 :: Integer)
  -- exit once done, don't check anything
  exitSuccess