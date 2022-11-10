module Lib
  ( start,
  )
where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

start :: IO ()
start = simpleMain ui
