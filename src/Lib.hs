module Lib (run) where

import           Data.Function  ((&))
import           Functions      (advance, h)
import           Graphics.Gloss
import           Linear
import           Particle

window :: Display
window = InWindow "Water" (1600, 900) (10, 10)

background :: Color
background = black

drawParticle :: Particle -> Picture
drawParticle part = (particle <> hcircle)
  & translate x y
  where
    -- uncomment to see h-circles
    hcircle = blank -- circle (realToFrac h) & Graphics.Gloss.color blue
    particle
      = thickCircle (realToFrac h / 2) (realToFrac h)
      & Graphics.Gloss.color (Particle.color part)
    i :: Double
    j :: Double
    V2 i j = position part
    x :: Float
    y :: Float
    (x, y) = (realToFrac i,  realToFrac j)

drawAll :: Water -> Picture
drawAll water = scale 0.5 0.5 (drawFrame <> foldMap drawParticle water)

drawFrame :: Picture
drawFrame
  = Graphics.Gloss.color white (rectangleSolid (1610 + realToFrac(2 * h)) (910 + realToFrac(2 * h)))
 <> Graphics.Gloss.color black (rectangleSolid (1600 + realToFrac(2 * h)) (900 + realToFrac(2 * h)))

run :: IO ()
run = simulate window background 30 initialState drawAll adv
  where
    adv _ _ = advance
