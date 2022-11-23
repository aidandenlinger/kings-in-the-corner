module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style as BS -- (unicodeRounded, bsVertical)
import Brick.Widgets.Center
import CardTypes
import Data.List (foldl1')
import Data.List.Index (modifyAt)
import Graphics.Vty (Event (..), Key (..))
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color (blue, green)
import System.Exit (exitSuccess)

-- dimension of cards
cardWidth :: Int
cardWidth = 4

cardHeight :: Int
cardHeight = 3

handPadding :: Int
handPadding = 1

cardSize :: (Int, Int)
cardSize = (cardWidth, cardHeight)

-- stolen from
-- <https://github.com/ambuc/solitaire/blob/0ada6e445c85f2f61c15081be49a99df6e272d29/src/Render.hs#L18>
-- given a widget, sets the border to be rounded :)
cardStyle :: Widget n -> Widget n
cardStyle = withBorderStyle BS.unicodeRounded . border

italicStyle :: Widget n -> Widget n
italicStyle = withBorderStyle custom . border

-- Don't use directly, used by cardWidget and cardWidgetHalf
createCard :: (Widget n1 -> Widget n2) -> String -> Widget n2
createCard centerFunc text =
  cardStyle $
    setAvailableSize cardSize $
      centerFunc $
        str text

custom :: BS.BorderStyle
custom =
  BS.BorderStyle
    { BS.bsCornerTL = '╒',
      BS.bsCornerTR = '╕',
      BS.bsCornerBR = '╛',
      BS.bsCornerBL = '╘',
      BS.bsHorizontal = '─',
      BS.bsVertical = '/'
    }

cardWidget :: String -> Widget ()
cardWidget = createCard center

cardWidgetNorthSouth :: String -> Widget ()
cardWidgetNorthSouth text = padLeftRight 30 $ cardStyle $ setAvailableSize cardSize $ center $ str text

cardWidgetNorthSouthBottom :: String -> Widget ()
cardWidgetNorthSouthBottom text = padLeftRight 30 $ cardStyle $ setAvailableSize (4, 2) $ center $ str text

cardWidgetEastWest :: String -> Widget ()
cardWidgetEastWest text = padLeftRight 13 $ cardStyle $ setAvailableSize cardSize $ center $ str text

cardWidgetEastWestBottom :: String -> Widget ()
cardWidgetEastWestBottom text = padLeftRight 13 $ cardStyle $ setAvailableSize (4, 2) $ center $ str text

-- centers the text horizontally, then fills bottom to get full card shape
cardWidgetHalf :: String -> Widget n
cardWidgetHalf n =
  cropBottomTo halfCardHeight $
    createCard (padBottom Max . hCenter) n
  where
    halfCardHeight = max 2 (cardHeight `div` 2)

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

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleEvent (sel, place, numCards) (VtyEvent (EvKey KLeft _)) =
  continue ((sel - 1) `mod` numCards, place, numCards)
handleEvent (sel, place, numCards) (VtyEvent (EvKey KRight _)) =
  continue ((sel + 1) `mod` numCards, place, numCards)
-- Up and Down move between decks
handleEvent (sel, place, numCards) (VtyEvent (EvKey KUp _)) =
  continue (sel, (place + 1) `mod` 4, numCards)
handleEvent (sel, place, numCards) (VtyEvent (EvKey KDown _)) =
  continue (sel, (place - 1) `mod` 4, numCards)
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
handleEvent s _ = continue s

playerCards :: [Card]
playerCards = [Card R5 Heart, Card R6 Club, Card R7 Heart]

board :: [String]
board = map show [Card R8 Spade, Card R9 Spade, Card R6 Club, Card RK Diamond, Card RQ Heart, Card RJ Diamond]

draw :: GameState -> Widget ()
draw (sel, place, _) =
  topPiles
    <=> playerHand
  where
    topPiles =
      cropBottomBy 2 (cardWidgetNorthSouthBottom (board !! 3)) -- top pile, bottom card
        <=> vBox (checkPlace 0 [cardWidgetNorthSouth (head board)]) -- top pile, top card
        <=> ( cropBottomBy 2 (cardWidgetEastWestBottom (board !! 3)) -- left pile, bottom card
                <+> cropBottomBy 2 (cardWidgetEastWestBottom (board !! 3)) -- right pile, bottom card
            )
        <=> ( hBox (checkPlace 3 [cardWidgetEastWest (board !! 1)]) -- left pile, top card
                <+> hBox (checkPlace 1 [cardWidgetEastWest (board !! 2)]) -- right pile, top card
            )
        <=> cropBottomBy 2 (cardWidgetNorthSouthBottom (board !! 5)) -- bottom pile, bottom card
        <=> vBox (checkPlace 2 [cardWidgetNorthSouth (board !! 3)]) -- bottom pile, top card
      where
        checkPlace p w
          | place == p = map placeCard w
          | otherwise = w
    playerHand = center (foldl1' (<+>) (modifyAt sel isSelected (map (cardWidget . show) playerCards))) -- player hand

gameStart :: IO ()
gameStart = do
  let app =
        App
          { -- given a state, return list of widgets to draw. in this case,
            -- state is the index of the card selected and the number of cards.
            -- make the playerHand a series of widgets, make the selected card
            -- selected, then use a fold to combine them all horizontally, and
            -- finally vertically append some text that states what is selected
            appDraw = \s -> [draw s],
            -- given state and an event, describe how to change state. the app
            -- is then redrawn
            appHandleEvent = handleEvent,
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
  _ <- defaultMain app (0, 0, length playerCards)
  -- exit once done, don't check anything
  exitSuccess