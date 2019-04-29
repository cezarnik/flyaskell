{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Functions where

import           Linear   (V2 (..), distance, (^*), (^/))
import           Particle


type DensPart = (Double, Particle)
-- p
unitVector :: Coord -> Coord -> Coord
unitVector start end
  | start == end = V2 0 0
  | otherwise = (end - start) ^/ (distance start end)

calcPressurePoint :: Double -> Double
calcPressurePoint p = k * (p - p0)
  where
    k = 0.01
    p0 = 0.1

-- Must be from 0 to 1
attenuationBounceCoef :: Double
attenuationBounceCoef = 0.5

gravityCoef :: Double
gravityCoef = 2


-- 1.0 not bad
tensionCoef :: Double
tensionCoef = 5

h :: Double
h = 20

viscocityCoef :: Double
viscocityCoef = -2



gravityForce :: Coord
gravityForce = (V2 0 (-1)) ^* gravityCoef

calcAbstractForcePoint :: (DensPart -> DensPart -> Coord) -> [DensPart] -> DensPart -> Coord
calcAbstractForcePoint forceCalculator densWater particle = sum (map g densWater)
  where
    g :: DensPart -> Coord
    g = forceCalculator particle


calcPressureForcePoint :: [DensPart] -> DensPart -> Coord
calcPressureForcePoint = calcAbstractForcePoint calcPressureForceBetweenPoints

calcViscosityForcePoint :: [DensPart] -> DensPart -> Coord
calcViscosityForcePoint = calcAbstractForcePoint calcViscosityForceBetweenPoints

calcTensionForcePoint :: [DensPart] -> DensPart -> Coord
calcTensionForcePoint = calcAbstractForcePoint calcTensionForceBetweenPoints


-- force from part2 on part1
calcPressureForceBetweenPoints :: DensPart -> DensPart -> Coord
calcPressureForceBetweenPoints (dens1, part1) (dens2, part2)
  | k /= 0 = unit ^* ((mass part2) * (pressure1 + pressure2) / (2 * dens2) * k)
  | otherwise = 0
  where
    k = gradWPoly len
    unit = unitVector (position part1) (position part2)
    len = distance (position part1) (position part2)
    pressure1 = calcPressurePoint dens1
    pressure2 = calcPressurePoint dens2


calcViscosityForceBetweenPoints :: DensPart -> (Double,Particle) -> Coord
calcViscosityForceBetweenPoints (_dens1,part1) (dens2,part2)
  | k /= 0 = (v1 - v2) ^* (m2 * viscocityCoef / dens2 * k)
  | otherwise = 0
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
wDens r
  | r <= h    = (1-r/h) ^ 2
  | otherwise = 0

wPoly :: Double -> Double
wPoly r
  | r <= h = 315 / 64 / pi / h^9 * (h^2 - r^2)^3
  | otherwise = 0

gradWPoly :: Double -> Double
gradWPoly r
  | r <= h = -315 / 64 / pi / h^9 * 6 * r * (h^2 - r^2) ^ 2
  | otherwise = 0

hessWPoly :: Double -> Double
hessWPoly r
  | r <= h = 315 / 64 / pi / h^9 * 6 * (h^2 - r^2) * (4 * r^2 - (h^2 - r^2))
  | otherwise = 0

applyHorizontalBound :: Particle -> Particle
applyHorizontalBound particle
  | py < -450 = particle { position = (V2 px (-900 - py)), velocity = (V2 vx (-vy * attenuationBounceCoef)) }
  | py > 450 =  particle { position = (V2 px (900 - py)), velocity = (V2 vx (-vy * attenuationBounceCoef)) }
  | otherwise = particle
  where
    V2 px py = position particle
    V2 vx vy = velocity particle

applyVerticalBound :: Particle -> Particle
applyVerticalBound particle
  | px < -800 = particle { position = (V2 (-1600 - px) py), velocity = (V2 (-vx * attenuationBounceCoef) vy) }
  | px > 800  = particle { position = (V2 (1600 - px) py), velocity = (V2 (-vx * attenuationBounceCoef) vy) }
  | otherwise = particle
  where
    V2 px py = position particle
    V2 vx vy = velocity particle

applyBound :: Particle -> Particle
applyBound = applyHorizontalBound . applyVerticalBound


advance :: Water -> Water
advance water = map g densAndWater
  where
    densAndWater :: [DensPart]
    densAndWater = zip densities water
    densities :: [Double]
    densities = getDensity water
    pressureForce :: DensPart -> Coord
    pressureForce = calcPressureForcePoint densAndWater
    viscosityForce :: DensPart -> Coord
    viscosityForce = calcViscosityForcePoint densAndWater
    tensionForce :: (Double, Particle) -> Coord
    tensionForce = calcTensionForcePoint densAndWater
    g :: DensPart -> Particle
    g (dens, part) = applyBound part { position=position part + velocity part
      , velocity=velocity part + gravityForce + ((pressureForce (dens, part) +
          viscosityForce (dens, part) + tensionForce (dens,part)) ^/ (mass part))}


calcDensityPoint :: Particle -> Water -> Double
calcDensityPoint particle water = sum (map g water)
  where
    g :: Particle -> Double
    g part
      | k /= 0 = k * (mass part)
      | otherwise = 0
      where
        k = wDens (distance (position part) (position particle))

getDensity :: Water -> [Double]
getDensity water = map g water
  where
    g :: Particle -> Double
    g part = calcDensityPoint part water
