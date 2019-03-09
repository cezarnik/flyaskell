module Lib (run) where

import Functions (advance)
import Particle
import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (1600, 900) (10, 10)

background :: Color
background = white

drawAll :: Water -> Picture
drawAll list = foldl g blank list
  where
    g :: Picture -> Particle -> Picture
    g pic part = pic <> drawParticle part

run :: IO ()
run = animate window background drawing
