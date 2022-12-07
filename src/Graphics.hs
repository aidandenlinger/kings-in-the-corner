{-# LANGUAGE TemplateHaskell #-}

module Graphics (gameStart) where

import Brick
import Brick.Widgets.Border (border, borderWithLabel, vBorder)
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
    currentPlayer = withBorderStyle BS.unicode . border $ hCenter $ str $ "Player " ++ show ((getCurrP gs) + 1) ++ "'s turn"
    --currentPlayer = hCenter $ str $ "Currently playing: Player " ++ show (getCurrP gs) ++ " Selected: " ++ show (gs ^. selpileft, gs ^. selpilefi, gs ^. selcdidx) ++ " Looking: " ++ show (gs ^. looking)

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

keyHelpSidebar :: GameState -> Widget ()
keyHelpSidebar gs =
    (translateBy (Location(gs ^. keyHelp)) $
    withBorderStyle BS.unicode . border $
    setAvailableSize (20, 45) $
    strWrap "Press UP arrow to select piles, DOWN to return to hand\n\nPress L/R arrows to select\n\nPress enter to make a selection\n\nPress 'n' to end your turn\n\nPress 'q' to close help")
    <=> (withBorderStyle BS.unicode . border $ setAvailableSize (20, 45) $ strWrap ("Helpful Hint: Place alternating black and red cards on piles in descending order and press 'n' to complete turn!"))

startScreen :: GameState -> Widget ()
startScreen gs =
    translateBy (Location(gs ^. welcome)) $
    joinBorders $
    withBorderStyle BS.unicode $
    borderWithLabel (str "Kings in the Corner") $
    ( ( center (str "How to play Kings in the Corner:\n") <=> (strWrap "Object of the Game: Players try to get rid of their cards by playing alternate red and black cards in descending order.\n\nThe Deal: Deal seven cards to each player. Place the remaining cards in the middle of the table as a stockpile. Then turn the four top cards over, placing one to the north, south, east, and west. These will be the foundation piles.\n\nThe Play: The player to the left of the dealer begins by drawing one card from the center stockpile. The player may make as many valid plays as are possible during their turn to get rid of as many cards as possible from their hand. Once there are no more valid moves, it’s the next player’s turn. Each player begins their turn by drawing a card from the center stockpile and making as many valid moves as they can.\n\nValid Moves: Play a card (or sequence of cards) on a foundation pile. To play cards on a foundation pile, the card played must be immediately below the foundation card in rank and of the opposite color (red or black). For example, if a 9♥ is on the foundation pile, then the next card face played must be 8♣ or 8♠. A sequence of cards may also be played, but all the cards in the sequence must obey the lower rank and opposite color rules. Aces are always the lowest cards.\n\nPlay a “King in the corner”: Kings are the only cards that can be played in the corner spaces. Once a King is played, players may then lay off cards on that pile like any other foundation pile. Move an entire foundation pile onto another pile, if the bottom card of that recipient pile and the top card of the moving pile creates a valid sequence. Play any card or sequence of cards on a vacated foundation pile.\n\nHow to Win: The first player to lay off all of their cards wins.\n\n")) <+>
    vBorder <+> 
    center (translateBy (Location(9, 0)) (cardWidget "♥️") <=> ((padLeftRight 4 (cardWidget "K♠")) <+> cardWidget "K♣︎") <=> (translateBy (Location(9, 0)) (cardWidget "♦️")) <=>  (translateBy (Location(3,0))  (padTopBottom 5 (str "Kings in the Corner")) <=>  (str "Press Spacebar to begin playing")) )  )

--- GAME START

gameStart :: IO ()
gameStart = do
  let app =
        App
          { -- given a state, return list of widgets to draw.
            appDraw = \s -> [startScreen s, keyHelpSidebar s, draw s],
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
