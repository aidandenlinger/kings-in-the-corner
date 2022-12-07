{-# LANGUAGE TemplateHaskell #-}

import System.Random
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head, set )
import Lens.Micro.TH (makeLenses)

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import CardTypes
import GamePlay
import Utils

-------------------------------------------------------------------------------
-- Convert data types from CardTypes to Lenses for easy access

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

------------------------------------------------------------------------------- 

-- Initialize game variables to be used with testing
-- Initialize game state

getGSt :: Int -> Int -> GSt
getGSt nPlayer seedv = initGSt nPlayer (nPlayer - 1) (mkStdGen seedv)


main :: IO ()
main = do
  utilsTest
  initGStTest
  gamePlayTest

-- exampleTest :: IO ()
-- exampleTest = hspec $ do
--   describe "Prelude.head" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)

--     it "returns the first element of an *arbitrary* list" $
--       property $
--         \x xs -> head (x : xs) == (x :: Int)

--     it "throws an exception if used with an empty list" $ do
--       evaluate (head []) `shouldThrow` anyException

utilsTest :: IO ()
utilsTest = hspec $ do
  describe "Utils Functions" $ do
    it "assignColor assigns black to spade" $ do
      assignColor Spade `shouldBe` (Black :: Color)
    it "assignColor assigns red to heart" $ do
      assignColor Heart `shouldBe` (Red :: Color)
    it "updateToPlay updates the toplay in gamestate" $
      property $
        \x y z -> (updateToPlay z (getGSt ((x `mod` 4) + 1) y)) ^. toplay == (z :: Int)
    it "updateSelCardIdx updates the selected card idx in gamestate" $
      property $
        \x y z -> (updateSelCardIdx z (getGSt ((x `mod` 4) + 1) y)) ^. selcdidx == (z :: Maybe Int)

initGStTest :: IO ()
initGStTest = hspec $ do
  describe "Game State Initialization" $ do
    it "Initializes n player hands for n players" $
      property $
        \x y -> length (getPHands (getGSt ((x `mod` 4) + 1) y)) == (((x `mod` 4) + 1) :: Int)
    it "Initializes player hand of length 7" $
      property $
        \x y -> length ((head (getPHands (getGSt ((x `mod` 4) + 1) y))) ^. cards) == (nCardsInHand :: Int)
    it "Initializes 4 center piles always" $
      property $
        \x y -> length (((getGSt ((x `mod` 4) + 1) y)) ^. field . centerPiles) == 4
    it "Initializes 4 corner piles always" $
      property $
        \x y -> length (((getGSt ((x `mod` 4) + 1) y)) ^. field . cornerPiles) == 4

gamePlayTest :: IO ()
gamePlayTest = hspec $ do
  describe "Game Play Functions" $ do
    it "Next card returns true for cards from different colors" $
      property $
        \r1 s1 r2 s2 -> not (isNextCard (Card r1 s1) (Card r2 s2)) || (assignColor s1 /= assignColor s2)
    it "Next card returns true for cards with successive rank" $
      property $
        \r1 s1 r2 s2 -> not (isNextCard (Card r1 s1) (Card r2 s2)) || (succ r1 == r2)