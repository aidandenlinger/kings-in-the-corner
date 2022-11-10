module Main (main) where

import Brick

ui :: Widget ()
ui = str "this is aidan's demo :0"

main :: IO ()
main = simpleMain ui