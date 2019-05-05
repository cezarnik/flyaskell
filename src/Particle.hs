module Particle where

import           Graphics.Gloss hiding (color)
import           Linear         (V2 (..))

type Coord  = V2 Double

data Particle = Particle
  { color    :: Color
  , position :: Coord
  , velocity :: Coord -- FIXME: this is not a coordinate
  , mass     :: Double
  } deriving (Show)

type Water = [Particle]

rad :: Integer
rad = 5

mkParticle :: Coord -> Particle
mkParticle coord = Particle
  { color     = withAlpha 0.5 white
  , position  = coord
  , velocity  = V2 0 0
  , mass      = 0.01
  }

initialState :: Water
initialState = [ mkParticle (V2 x y) | x <- [-70, -65..70] , y <- [-70, -65..70] ]
