module Simulator (simulatePPS) where

import qualified Graphics.Gloss as Gloss
import qualified Data.Vector as Vec
import Data.Maybe
import Defs
import MathHelp

displayPartAlpha :: Float -> Particle -> Gloss.Picture
displayPartAlpha alpha p = ourboi
  where
    ourColor = Gloss.withAlpha alpha (color p)
    ourboi = Gloss.color ourColor $ uncurry Gloss.translate (pos p) $ Gloss.circleSolid (size p)

displayParticle :: Particle -> Gloss.Picture
displayParticle = Gloss.pictures . go 1 . Just
  where
    go :: Float -> Maybe Particle -> [Gloss.Picture]
    go _ Nothing = []
    go alpha (Just original) = displayPartAlpha alpha original : go (alpha * 1/2) (past original)

displayPPS :: PPS -> Gloss.Picture
displayPPS = Gloss.pictures . Vec.toList . fmap displayParticle

moveParticle :: Float -> Particle -> Particle
moveParticle dt original = changePos (posUpdate (polarToCart $ vel original)) original
  where
    posUpdate (dvx, dvy) (x, y) = (x + dvx * dt, y + dvy * dt)

versionsParticle :: Int -> Particle -> Particle
versionsParticle n = fromJust . listToParticle . take n . particleToList . Just

phiParticle :: Config -> Float -> PPS -> Particle -> Particle
phiParticle conf dt pps original = changeVel changeIt original
  where
    ourAlpha = (alph conf)
    ourBeta = (beta conf)
    (n, leftN, rightN) = getNeighbors conf pps original
    changeIt (vr, vphi) = (vr, vphi + dt * (ourAlpha + (fromIntegral n) * ourBeta * fromIntegral (sgn (rightN - leftN))))

getNeighbors :: Config -> PPS -> Particle -> (Int, Int, Int)
getNeighbors conf pps original = (n, leftN, rightN)
  where
    r2 = (radius2 conf)
    n = length filteredPPS
    leftN = length leftPPS
    rightN = n - leftN
    leftPPS = filter (leftOf (pos original) (vel original)) filteredPPS
    filteredPPS = filter (nearTo (pos original)) (Vec.toList $ fmap pos pps)
    nearTo pos1 pos2 = (distance2 pos1 pos2) < r2
    leftOf (x1, y1) (_, vphi) (x2, y2) = let
      theta = atan2 (y2 - y1) (x2 - x1)
      change = theta - vphi
      in (0 < change) && (change < pi)

updateParticle :: Config -> PPS -> Float -> Particle -> Particle
updateParticle conf pps dt = versionsParticle (howManyVersions conf) . moveParticle dt . phiParticle conf dt pps

updatePPS :: Config -> Float -> PPS -> PPS
updatePPS conf dt pps = fmap (updateParticle conf pps dt) pps

simulatePPS :: Config -> IO ()
simulatePPS conf = Gloss.simulate (confWindow conf) (backgroundColor conf) 10 (initpps conf) displayPPS (const $ updatePPS conf)


