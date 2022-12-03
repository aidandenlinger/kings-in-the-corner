module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border, borderWithLabel, vBorder)
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import CardTypes
import Data.List.Index (modifyAt)
import Graphics.Vty (Event (..), Key (..))
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color (blue, green)
import System.Exit (exitSuccess)

--- CONSTANTS

-- dimension of cards
cardWidth :: Int
cardWidth = 4

cardHeight :: Int
cardHeight = 3

cardSize :: (Int, Int)
cardSize = (cardWidth, cardHeight)

-- Padding values
handPadding :: Int
handPadding = 1

pileHorizPadding :: Int
pileHorizPadding = 4

pileVertPadding :: Int
pileVertPadding = 0

type GameState = (Int, Int, Int, (Int, Int))

--- MAIN DRAW FUNCTION

-- Given the GameState, return the widget to draw
draw :: GameState -> Widget ()
draw (sel, place, _, _) = vBox [topPiles, playerHand]
  where
    -- TODO: Don't use hardcoded values, get piles from gamestate
    topPiles = createTopPiles place [topPile, rightPile, bottomPile, leftPile]
    playerHand = createPlayerHand sel playerCards

--- PILES

-- Given the top, right, bottom, and left pile, along with the currently
-- selected pile, return the widget for the piles of the board.
-- This expects a list of four elements - top pile, right pile, bottom pile,
-- left pile. TODO: better datatype in the future, order shouldn't matter
-- TODO: Don't use an Int to signify what pile we're pointing at, use an enum
createTopPiles :: Int -> [[Card]] -> Widget ()
createTopPiles place piles
  | length piles == 4 =
      vBox $
        map
          ( padTopBottom pileVertPadding
              . hCenter
              . hBox
              . map (padLeftRight pileHorizPadding)
          )
          -- TODO: fill corner and middle decks with real values
          [ [ (cardWidgetItalic "top\nleft"), topWidget, (cardWidgetItalic "top\nrigh\nt")],
            [leftWidget, translateBy (Location(0,1)) (cardWidget "deck"), rightWidget],
            [translateBy (Location(0,2)) (cardWidgetItalic "bot\nleft"), bottomWidget, translateBy (Location(0,2)) (cardWidgetItalic "bot\nrigh\nt")]
          ]
  where
    [topWidget, rightWidget, bottomWidget, leftWidget] =
      modifyAt place placeCard $
        map pileToOverlap piles
createTopPiles _ _ = error "hardcoded for four piles for now :)"

--- Given a pile, return a widget showing the top and bottom card of the pile
pileToOverlap :: [Card] -> Widget ()
pileToOverlap pile = vBox [cardWidgetHalf bottomCard, cardWidget topCard]
  where
    bottomCard = show (last pile)
    topCard = show (head pile)

-- Given an index of selected card and list of cards, return a widget of the
-- player's hand.
createPlayerHand :: Int -> [Card] -> Widget ()
createPlayerHand sel hand =
  center $
    hBox $
      modifyAt sel isSelected $
        map (padLeftRight handPadding . cardWidget . show) hand

--- CARDS AND STYLE

-- Don't use directly, used by cardWidget and cardWidgetHalf
createCard :: (Widget n1 -> Widget n2) -> String -> Widget n2
createCard centerFunc text =
  cardStyle $
    setAvailableSize cardSize $
      centerFunc $
        str text

createCardItalic :: (Widget n1 -> Widget n2) -> String -> Widget n2
createCardItalic centerFunc text =
  italicStyle $
    setAvailableSize cardSize $
      centerFunc $
        str text
cardWidget :: String -> Widget ()
cardWidget = createCard center

cardWidgetItalic :: String -> Widget ()
cardWidgetItalic = createCardItalic center

-- centers the text horizontally, then fills bottom to get full card shape
cardWidgetHalf :: String -> Widget n
cardWidgetHalf n =
  cropBottomTo halfCardHeight $
    createCard (padBottom Max . hCenter) n
  where
    halfCardHeight = max 2 (cardHeight `div` 2)

-- stolen from
-- <https://github.com/ambuc/solitaire/blob/0ada6e445c85f2f61c15081be49a99df6e272d29/src/Render.hs#L18>
-- given a widget, sets the border to be rounded :)
cardStyle :: Widget n -> Widget n
cardStyle = withBorderStyle BS.unicodeRounded . border

italicStyle :: Widget n -> Widget n
italicStyle = withBorderStyle custom . border
  where
    custom =
      BS.BorderStyle   { BS.bsCornerTL = toEnum 0x256D
                  , BS.bsCornerTR = toEnum 0x256E
                  , BS.bsCornerBR = toEnum 0x256F
                  , BS.bsCornerBL = toEnum 0x2570
                  , BS.bsHorizontal = '─'
                  , BS.bsVertical = '•'
                   }

-- attributes that widgets can use
attrs :: [(AttrName, Attr)]
attrs = [(attrName "selected_card", fg blue), (attrName "place_card", fg green)]

-- marks a card as selected
isSelected :: Widget n -> Widget n
isSelected = withAttr (attrName "selected_card")

-- where to place card
placeCard :: Widget n -> Widget n
placeCard = withAttr (attrName "place_card")

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

--- EVENT HANDLING

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleEvent (sel, place, numCards, r) (VtyEvent (EvKey KLeft _)) =
  continue ((sel - 1) `mod` numCards, place, numCards, r)
handleEvent (sel, place, numCards, r) (VtyEvent (EvKey KRight _)) =
  continue ((sel + 1) `mod` numCards, place, numCards, r)
-- Up and Down move between decks
handleEvent (sel, place, numCards, r) (VtyEvent (EvKey KUp _)) =
  continue (sel, (place + 1) `mod` 4, numCards, r)
handleEvent (sel, place, numCards, r) (VtyEvent (EvKey KDown _)) =
  continue (sel, (place - 1) `mod` 4, numCards, r)
handleEvent (sel, place, numCards, r) (VtyEvent (EvKey KEnter _)) =
  continue (sel, place, numCards, (50, 50))
-- Esc quits game
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s

ui :: GameState -> Widget ()
ui (sel, place, _, (r1, r2)) =
    translateBy (Location(r1, r2)) $
    joinBorders $
    withBorderStyle BS.unicode $
    borderWithLabel (str "Kings in the Corner") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

--- GAME START

gameStart :: IO ()
gameStart = do
  let app =
        App
          { -- given a state, return list of widgets to draw.
            appDraw = \s -> [ui s, draw s],
            -- given state and an event, describe how to change state. the app
            -- is then redrawn
            appHandleEvent = handleEvent,
            -- returns an EventM that runs at app start, this is a demo,
            -- there's nothing to do at start, return
            appStartEvent = return,
            -- holds attributes that can be applied to elements
            appAttrMap = const $ attrMap defAttr attrs,
            -- don't show cursor
            appChooseCursor = neverShowCursor
          }
  -- use defaultMain to start our app
  -- ignore the final returned state for now, because this app doesn't store
  -- anything
  _ <- defaultMain app (0, 0, length playerCards, (0, 0))
  -- exit once done, don't check anything
  exitSuccess