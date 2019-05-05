{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Functions where

import           Linear   (V2 (..), distance, (^*), (^/))
import           Particle
import           QuadTree
import           Consts

type DensPart = (Double, Particle)
-- p
unitVector :: Coord -> Coord -> Coord
unitVector start end
  | start == end = V2 0 0
  | otherwise = (end - start) ^/ (distance start end)

calcPressurePoint :: Double -> Double
calcPressurePoint p = k * (p - p0)
  where
    k = 0.2
    p0 = 0.01

-- Must be from 0 to 1
attenuationBounceCoef :: Double
attenuationBounceCoef = 0.1

gravityCoef :: Double
gravityCoef = 0


-- 1.0 not bad
tensionCoef :: Double
tensionCoef = 5

h :: Double
h = 15

viscocityCoef :: Double
viscocityCoef = -2

gravityForce :: Coord
gravityForce = (V2 0 (-1)) ^* gravityCoef

-- calcAbstractForcePoint :: (DensPart -> DensPart -> Coord) -> [DensPart] -> DensPart ->  Coord
calcAbstractForcePoint forceCalculator densWater qt (dens, part) = sum (map g near)
  where
    g :: (DensPart, Int) -> Coord
    g (densNear, cnt) = (forceCalculator (dens, part) densNear) ^* (fromIntegral cnt)
    near = getResults h (position part) qt


-- calcPressureForcePoint :: [DensPart] -> DensPart -> Coord
calcPressureForcePoint = calcAbstractForcePoint calcPressureForceBetweenPoints

-- calcViscosityForcePoint :: [DensPart] -> DensPart -> Coord
calcViscosityForcePoint = calcAbstractForcePoint calcViscosityForceBetweenPoints

-- calcTensionForcePoint :: [DensPart] -> DensPart -> Coord
calcTensionForcePoint = calcAbstractForcePoint calcTensionForceBetweenPoints


-- force from part2 on part1
calcPressureForceBetweenPoints :: DensPart -> DensPart -> Coord
calcPressureForceBetweenPoints (dens1, part1) (dens2, part2) = unit ^* ((mass part2) * (pressure1 + pressure2) / (2 * dens2) * k)
  where
    k = gradWPoly len
    unit = unitVector (position part1) (position part2)
    len = distance (position part1) (position part2)
    pressure1 = calcPressurePoint dens1
    pressure2 = calcPressurePoint dens2


calcViscosityForceBetweenPoints :: DensPart -> (Double,Particle) -> Coord
calcViscosityForceBetweenPoints (_dens1,part1) (dens2,part2) = (v1 - v2) ^* (m2 * viscocityCoef / dens2 * k)
  where
    k = hessWPoly (distance (position part1) (position part2))
    v1 = velocity part1
    v2 = velocity part2
    m2 = mass part2


calcTensionForceBetweenPoints :: DensPart -> DensPart -> Coord
calcTensionForceBetweenPoints (_dens1, part1) (dens2, part2) = unit ^* ((mass part2) * tensionCoef / dens2 *
  (hessWPoly (distance (position part1) (position part2))))
  where
    unit =  (unitVector (position part1) (position part2))



wDens  :: Double -> Double
wDens r = (1-r/h) ^ 2

wPoly :: Double -> Double
wPoly r = 315 / 64 / pi / h^9 * (h^2 - r^2)^3

gradWPoly :: Double -> Double
gradWPoly r = -315 / 64 / pi / h^9 * 6 * r * (h^2 - r^2) ^ 2

hessWPoly :: Double -> Double
hessWPoly r = 315 / 64 / pi / h^9 * 6 * (h^2 - r^2) * (4 * r^2 - (h^2 - r^2))

applyHorizontalBound :: Particle -> Particle
applyHorizontalBound particle
  | py < -half = particle { position = (V2 px (-h - py)), velocity = (V2 vx (-vy * attenuationBounceCoef)) }
  | py > half =  particle { position = (V2 px (h - py)), velocity = (V2 vx (-vy * attenuationBounceCoef)) }
  | otherwise = particle
  where
    h = fromIntegral height
    half = h / 2
    V2 px py = position particle
    V2 vx vy = velocity particle

applyVerticalBound :: Particle -> Particle
applyVerticalBound particle
  | px < -half = particle { position = (V2 (-w - px) py), velocity = (V2 (-vx * attenuationBounceCoef) vy) }
  | px > half  = particle { position = (V2 (w - px) py), velocity = (V2 (-vx * attenuationBounceCoef) vy) }
  | otherwise = particle
  where
    w = fromIntegral width
    half = w / 2
    V2 px py = position particle
    V2 vx vy = velocity particle

applyBound :: Particle -> Particle
applyBound = applyHorizontalBound . applyVerticalBound

getPositionOfDensPart :: DensPart -> Coord
getPositionOfDensPart (_, part) = position part

buildQuadTree water getpos = foldr g (QuadTree (V2 (-w / 2) (-h / 2),  V2 (w / 2) (h / 2)) Empty) water
  where
    w = fromIntegral width
    h = fromIntegral height
    g el qt = addElement qt el (getpos el)

advance :: Water -> Water
advance water = map g densAndWater
  where
    waterQuadTree = buildQuadTree water position
    densities :: [Double]
    densities = getDensity water waterQuadTree
    densAndWater :: [DensPart]
    densAndWater = zip densities water
    densAndWaterQuadTree = buildQuadTree densAndWater getPositionOfDensPart
    pressureForce :: DensPart -> Coord
    pressureForce = calcPressureForcePoint densAndWater densAndWaterQuadTree
    viscosityForce :: DensPart -> Coord
    viscosityForce = calcViscosityForcePoint densAndWater densAndWaterQuadTree
    tensionForce :: (Double, Particle) -> Coord
    tensionForce = calcTensionForcePoint densAndWater densAndWaterQuadTree
    g :: DensPart -> Particle
    g (dens, part) = applyBound part { position=position part + velocity part
      , velocity=velocity part + gravityForce + ((pressureForce (dens, part) +
          viscosityForce (dens, part) + tensionForce (dens,part)) ^/ (mass part))}


calcDensityPoint :: Particle -> [(Particle, Int)] -> Double
calcDensityPoint particle water = sum (map g water)
  where
    g :: (Particle, Int) -> Double
    g (part, cnt) = (fromIntegral cnt) * k * (mass part)
      where
        k = wDens (distance (position part) (position particle))

getDensity water qt = map g water
  where
    g :: Particle -> Double
    g part = calcDensityPoint part nearwater
      where
        nearwater = getResults h (position part) qt
