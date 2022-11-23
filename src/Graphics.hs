module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style as BS -- (unicodeRounded, bsVertical)
import Brick.Widgets.Center
import CardTypes
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
  where
    custom =
      BS.BorderStyle
        { BS.bsCornerTL = '╒',
          BS.bsCornerTR = '╕',
          BS.bsCornerBR = '╛',
          BS.bsCornerBL = '╘',
          BS.bsHorizontal = '─',
          BS.bsVertical = '/'
        }

-- Don't use directly, used by cardWidget and cardWidgetHalf
createCard :: (Widget n1 -> Widget n2) -> String -> Widget n2
createCard centerFunc text =
  cardStyle $
    setAvailableSize cardSize $
      centerFunc $
        str text

cardWidget :: String -> Widget ()
cardWidget = createCard center

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
-- Esc quits game
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s

-- Example hands
playerCards :: [Card]
playerCards = [Card R5 Heart, Card R6 Club, Card R7 Heart]

topPile :: [Card]
topPile = [Card R8 Spade, Card RK Diamond]

leftPile :: [Card]
leftPile = [Card R9 Spade, Card RK Diamond]

rightPile :: [Card]
rightPile = [Card R6 Club, Card RK Diamond]

bottomPile :: [Card]
bottomPile = [Card RK Diamond, Card RJ Diamond]

pileToOverlap :: [Card] -> Widget ()
pileToOverlap pile = vBox [cardWidgetHalf bottomCard, cardWidget topCard]
  where
    bottomCard = (show . last) pile
    topCard = (show . head) pile

-- Given a list of widgets, add that much space inbetween the elements
-- horizontally.
hPadList :: Int -> [Widget ()] -> [Widget ()]
hPadList _ [] = []
hPadList p (w : ws) = w : map (padLeft (Pad p)) ws

-- Given a list of widgets, add that much space inbetween the elements
-- vertically.
vPadList :: Int -> [Widget ()] -> [Widget ()]
vPadList _ [] = []
vPadList p (w : ws) = w : map (padTop (Pad p)) ws

-- This expects a list of four elements - top pile, right pile, bottom pile,
-- left pile. This should be not hardcoded in the future
createTopPiles :: Int -> [[Card]] -> Widget ()
createTopPiles place piles
  | length piles == 4 =
      vBox $
        vPadList (handPadding * 2) $
          map
            (hCenter . hBox . hPadList (handPadding * 9))
            [ [cardWidget "top\nleft", topWidget, cardWidget "top\nrigh\nt"],
              [leftWidget, cardWidget "deck", rightWidget],
              [cardWidget "bot\nleft", bottomWidget, cardWidget "bot\nrigh\nt"]
            ]
  where
    [topWidget, rightWidget, bottomWidget, leftWidget] =
      modifyAt place placeCard $
        map pileToOverlap piles
createTopPiles _ _ = error "hardcoded for four piles for now :)"

draw :: GameState -> Widget ()
draw (sel, place, _) =
  topPiles
    <=> playerHand
  where
    topPiles = createTopPiles place [topPile, rightPile, bottomPile, leftPile]
    playerHand =
      center $
        hBox $
          modifyAt sel isSelected $
            map (padAll handPadding . cardWidget . show) playerCards

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