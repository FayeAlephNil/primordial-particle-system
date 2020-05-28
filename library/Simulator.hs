module Simulator (simulatePPS) where

import qualified Graphics.Gloss as Gloss
import qualified Data.Vector as Vec
import Defs
import MathHelp

displayParticle :: Particle -> Gloss.Picture
displayParticle p = doOne 1 (pos p) <> (Gloss.color Gloss.white $ Gloss.line (past p))
  where
    -- n = 1 + length (past p)
    ourColor = color p
    doOne :: Float -> Position -> Gloss.Picture
    doOne alpha position = Gloss.color (Gloss.withAlpha alpha ourColor) $ uncurry Gloss.translate position $ Gloss.circleSolid (size p)

displayPPS :: PPS -> Gloss.Picture
displayPPS = Gloss.pictures . Vec.toList . fmap displayParticle

moveParticle :: Float -> Particle -> Particle
moveParticle dt original = changePos (posUpdate (polarToCart $ vel original)) original
  where
    posUpdate (dvx, dvy) (x, y) = (x + dvx * dt, y + dvy * dt)

versionsParticle :: Int -> Particle -> Particle
versionsParticle n original = original {past = newPast}
  where
    safeInit :: [a] -> [a]
    safeInit [] = []
    safeInit as = init as

    ourPast = past original
    newPast = pos original : if length ourPast < n then ourPast else (safeInit ourPast)

phiParticle :: Config -> Float -> PPS -> Particle -> Particle
phiParticle conf dt pps original = (changeVel changeIt original) { color = newColor }
  where
    newColor = (particleColor conf) n original
    ourAlpha = (alph conf)
    ourBeta = (beta conf)
    (n, leftN, rightN) = getNeighbors conf pps original
    changeIt (vr, vphi) = (vr, vphi + dt * (ourAlpha + (fromIntegral n) * ourBeta * fromIntegral (sgn (rightN - leftN))))

getNeighbors :: Config -> PPS -> Particle -> (Int, Int, Int)
getNeighbors conf pps original = (n, leftN, rightN)
  where
    r2 = (radius2 conf)
    n = length filteredPPS - 1
    leftN = length leftPPS
    rightN = n - leftN
    leftPPS = filter (leftOf (pos original) (vel original)) filteredPPS
    filteredPPS = filter (nearTo (pos original)) (Vec.toList $ fmap pos $ Vec.filter (/= original) pps)
    nearTo pos1 pos2 = (distance2 pos1 pos2) < r2
    leftOf (x1, y1) (_, vphi) (x2, y2) = let
      theta = atan2 (y2 - y1) (x2 - x1)
      change = theta - vphi
      in (0 < change) && (change < pi)

updateParticle :: Config -> PPS -> Float -> Particle -> Particle
updateParticle conf pps dt =  moveParticle dt . phiParticle conf dt pps . versionsParticle (howManyVersions conf)

updatePPS :: Config -> Float -> PPS -> PPS
updatePPS conf dt pps = fmap (updateParticle conf pps (speedUp conf * dt)) pps

simulatePPS :: Config -> IO ()
simulatePPS conf = Gloss.simulate (confWindow conf) (backgroundColor conf) 20 (initpps conf) displayPPS (const $ updatePPS conf)
