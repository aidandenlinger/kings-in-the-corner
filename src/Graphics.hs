{-# LANGUAGE TemplateHaskell #-}

module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import CardTypes
import Data.List.Index (modifyAt)
import Graphics.Vty (Event (..), Key (..))
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color (blue, green)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Exit (exitSuccess)
import System.Random (initStdGen)
import Utils

--- LENSES

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

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

type GameState = GSt

--- MAIN DRAW FUNCTION

-- Given the GameState, return the widget to draw
draw :: GameState -> Widget ()
draw gs = vBox [topPiles, playerHand]
  where
    -- TODO: Don't hardcode selected pile
    topPiles = createTopPiles 0 (gs ^. field)
    playerHand = createPlayerHand (gs ^. lookcd) (map (^. card) (((gs ^. field . phands) !! (gs ^. toplay)) ^. cards))

--- PILES

-- Given the top, right, bottom, and left pile, along with the currently
-- selected pile, return the widget for the piles of the board.
-- This expects a list of four elements - top pile, right pile, bottom pile,
-- left pile. TODO: better datatype in the future, order shouldn't matter
-- TODO: Don't use an Int to signify what pile we're pointing at, use an enum
-- TODO: Don't just always select 1st pile
createTopPiles :: Int -> Field -> Widget ()
createTopPiles place piles =
  vBox $
    map
      ( padTopBottom pileVertPadding
          . hCenter
          . hBox
          . map (padLeftRight pileHorizPadding)
      )
      -- TODO: fill corner and middle decks with real values
      [ [(cardWidgetItalic "top\nleft"), topWidget, (cardWidgetItalic "top\nrigh\nt")],
        [leftWidget, cardWidget "deck", rightWidget],
        [translateBy (Location (0, 2)) (cardWidgetItalic "bot\nleft"), bottomWidget, translateBy (Location (0, 2)) (cardWidgetItalic "bot\nrigh\nt")]
      ]
  where
    [topWidget, rightWidget, bottomWidget, leftWidget] =
      modifyAt place placeCard $
        map pileToOverlap centerPileCards

    centerPileCards :: [[Card]]
    centerPileCards = map (map (^. card) . (^. cards)) (piles ^. centerPiles)

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
      BS.BorderStyle
        { BS.bsCornerTL = toEnum 0x256D,
          BS.bsCornerTR = toEnum 0x256E,
          BS.bsCornerBR = toEnum 0x256F,
          BS.bsCornerBL = toEnum 0x2570,
          BS.bsHorizontal = '─',
          BS.bsVertical = '#'
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

--- EVENT HANDLING

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
-- Left and Right move between player cards
handleEvent gs (VtyEvent (EvKey KRight _)) =
  continue $ over lookcd (\b -> (b + 1) `mod` playerHandLength) gs
  where
    playerHandLength = length $ ((gs ^. field . phands) !! (gs ^. toplay)) ^. cards
handleEvent gs (VtyEvent (EvKey KLeft _)) =
  continue $ over lookcd (\b -> (b - 1) `mod` playerHandLength) gs
  where
    playerHandLength = length $ ((gs ^. field . phands) !! (gs ^. toplay)) ^. cards
-- Up and Down move between decks
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KUp _)) =
--   continue (sel, (place + 1) `mod` 4, numCards)
-- handleEvent (sel, place, numCards) (VtyEvent (EvKey KDown _)) =
--   continue (sel, (place - 1) `mod` 4, numCards)
-- Esc quits game
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- Everything else does not change state
handleEvent s _ = continue s

--- GAME START

gameStart :: IO ()
gameStart = do
  let app =
        App
          { -- given a state, return list of widgets to draw.
            appDraw = \s -> [draw s],
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
  -- Start a two player game from a random generator
  finalState <- defaultMain app . initGSt 2 <$> initStdGen
  final <- finalState -- need to access the gamestate,
                      -- otherwise haskell will be lazy
                      -- and not run the game at all :)
  -- exit once done, don't check anything
  exitSuccess