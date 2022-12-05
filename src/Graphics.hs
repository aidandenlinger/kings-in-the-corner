{-# LANGUAGE TemplateHaskell #-}

module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center
import CardTypes
import Data.List.Index (modifyAt)
import Graphics.Vty.Attributes (Attr, defAttr)
import Graphics.Vty.Attributes.Color
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Exit (exitSuccess)
import System.Random (initStdGen)
import Utils
import Events

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
draw gs = vBox [topPiles, playerHand, currentPlayer]
  where
    topPiles = createTopPiles (gs ^. looking) (gs ^. selpileft, gs ^. selpilefi) (getPiles gs)
    playerHand = createPlayerHand (gs ^. looking) (gs ^. selpileft, gs ^. selcdidx) (getCurrPCards gs)
    currentPlayer = hCenter $ str $ "Currently playing: Player " ++ show (getCurrP gs) ++ " Selected: " ++ show (gs ^. selpileft, gs ^. selpilefi, gs ^. selcdidx) ++ " Looking: " ++ show (gs ^. looking)

--- PILES

-- Given the top, right, bottom, and left pile, along with the currently
-- selected pile, return the widget for the piles of the board.
-- This expects a list of four elements - top pile, right pile, bottom pile,
-- left pile.
createTopPiles :: Look -> (Maybe PileType, Maybe Int) -> [[Card]] -> Widget ()
createTopPiles look selInfo piles =
  vBox $
    map
      ( padTopBottom pileVertPadding
          . hCenter
          . hBox
          . map (padLeftRight pileHorizPadding)
      )
      [ [topLeftWidget, topWidget, topRightWidget],
        [leftWidget, drawWidget, rightWidget],
        [bottomLeftWidget, bottomWidget, bottomRightWidget]
      ]
  where
    [topLeftWidget, topWidget, topRightWidget, leftWidget, _, rightWidget, bottomLeftWidget, bottomWidget, bottomRightWidget] =
      setSelected selInfo $
      setLooking look $
        map pileToOverlap piles

    -- created separately because we don't want to display what card draw is
    drawWidget = drawSelect look selInfo (cardWidget "deck")
      where
        drawSelect (PileLook 4) _ = isViewed
        drawSelect _ (Just DrawP, Just 0) = isSelected -- error case, shouldn't be possible
                                                       -- TODO: remove draw from hover/selection
        drawSelect _ _ = id

    setLooking (PileLook pileidx) = modifyAt pileidx isViewed
    setLooking _ = id

    setSelected (Just CornerP, Just 0) = modifyAt 0 isSelected
    setSelected (Just CenterP, Just 0) = modifyAt 1 isSelected
    setSelected (Just CornerP, Just 1) = modifyAt 2 isSelected
    setSelected (Just CenterP, Just 1) = modifyAt 3 isSelected
    setSelected (Just CenterP, Just 2) = modifyAt 5 isSelected
    setSelected (Just CornerP, Just 2) = modifyAt 6 isSelected
    setSelected (Just CenterP, Just 3) = modifyAt 7 isSelected
    setSelected (Just CornerP, Just 3) = modifyAt 8 isSelected
    setSelected _ = id

--- Given a pile, return a widget showing the top and bottom card of the pile
pileToOverlap :: [Card] -> Widget ()
pileToOverlap [] = emptyCardWidget
pileToOverlap [singleCard] = cardWidget (show singleCard)
pileToOverlap pile = vBox [cardWidgetHalf bottomCard, cardWidget topCard]
  where
    bottomCard = show (last pile)
    topCard = show (head pile)

-- Given an index of viewed card, selected card, and list of cards, return a widget of the
-- player's hand.
createPlayerHand :: Look -> (Maybe PileType, Maybe Int) -> [Card] -> Widget ()
createPlayerHand look sel hand =
  center $
    hBox $
      map (padLeftRight handPadding) $
      lookFunction look $
      selFunction sel $
        map (cardWidget . show) hand
  where
    lookFunction (PlayerLook idx) = modifyAt idx isViewed
    lookFunction _ = id

    selFunction (Just PlayerP, Just idx) = modifyAt idx isSelected 
    selFunction _ = id

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

emptyCardWidget :: Widget n
emptyCardWidget = createCard (padBottom Max . padRight Max) "    "

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
attrs = [(attrName "viewed_card", bg blue),
         (attrName "selected_card", bg green),
         (attrName "place_card", fg green)]

-- marks a card as selected
isViewed :: Widget n -> Widget n
isViewed = withAttr (attrName "viewed_card")

isSelected :: Widget n -> Widget n
isSelected = withAttr (attrName "selected_card")

-- where to place card
placeCard :: Widget n -> Widget n
placeCard = withAttr (attrName "place_card")

--- GAME START

gameStart :: IO ()
gameStart = do
  let app =
        App
          { -- given a state, return list of widgets to draw.
            appDraw = \s -> [draw s],
            -- given state and an event, describe how to change state. the app
            -- is then redrawn
            appHandleEvent = handleEvent, -- in Event.hs
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