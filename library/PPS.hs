-- | An example module.
module PPS (main, simulatePPS) where

import qualified Graphics.Gloss as Gloss
import qualified Data.Vector as Vec
import Data.Maybe

type Velocity = Gloss.Vector
type Position = Gloss.Point

r2 :: Float
r2 = 2500

data Particle = Particle
  {
    size :: Float,
    color :: Gloss.Color,
    pos :: Position,
    vel :: Velocity,
    past :: Maybe Particle
  }

type PPS = Vec.Vector Particle

changePos :: (Position -> Position) -> Particle -> Particle
changePos f p = Particle {
                                size = (size p),
                                color = (color p),
                                pos = f (pos p),
                                vel = (vel p),
                                past = (past p)
                              }

changeVel :: (Velocity -> Velocity) -> Particle -> Particle
changeVel f p = Particle {
                                size = (size p),
                                color = (color p),
                                pos = (pos p),
                                vel = f (vel p),
                                past = (past p)
                              }
                     
changePast :: Maybe Particle -> Particle -> Particle
changePast newPast p = Particle {
                                size = (size p),
                                color = (color p),
                                pos = (pos p),
                                vel = (vel p),
                                past = newPast
                              }

particleToList :: Maybe Particle -> [Particle]
particleToList Nothing = []
particleToList (Just p) = p : (particleToList (past p))

listToParticle :: [Particle] -> Maybe Particle
listToParticle [] = Nothing
listToParticle (p : ps) = Just $ changePast (listToParticle ps) p

window :: Gloss.Display
window = Gloss.InWindow "Nice Window" (200, 200) (10, 10)

background :: Gloss.Color
background = Gloss.black

drawing :: Gloss.Picture
drawing = Gloss.circle 80

displayPartAlpha :: Float -> Particle -> Gloss.Picture
displayPartAlpha alpha p = ourboi
  where
    ourColor = Gloss.withAlpha alpha (color p)
    ourboi = Gloss.color ourColor $ uncurry Gloss.translate (pos p) $ Gloss.circleSolid (size p)

displayParticle :: Particle -> Gloss.Picture
displayParticle = Gloss.pictures . go 1 . Just
  where
    go :: Float -> Maybe Particle -> [Gloss.Picture]
    go alpha Nothing = []
    go alpha (Just original) = displayPartAlpha alpha original : go (alpha * 1/2) (past original)

displayPPS :: PPS -> Gloss.Picture
displayPPS = Gloss.pictures . Vec.toList . fmap displayParticle

moveParticle :: Float -> Particle -> Particle
moveParticle dt original = changePos (posUpdate (vel original)) original
  where
    posUpdate (dvx, dvy) (x, y) = (x + dvx * dt, y + dvy * dt)

versionsParticle :: Int -> Particle -> Particle
versionsParticle n = fromJust . listToParticle . take n . particleToList . Just

norm2 :: Position -> Float
norm2 (x, y) = x ** 2 + y ** 2

norm :: Position -> Float
norm = sqrt . norm2

distance2 :: Position -> Position -> Float
distance2 (x1, y1) (x2, y2) = norm2 (x1 - x2, y1 - y2)

polarToCart :: (Float, Float) -> (Float, Float)
polarToCart (mag, theta) = (mag * cos theta, mag * sin theta)

cartToPolar :: (Float, Float) -> (Float, Float)
cartToPolar (x, y) = (norm (x, y), atan2 y x)

sgn :: Float -> Int
sgn 0 = 0
sgn x = if x > 0 then 1 else -1

phiParticle :: Float -> PPS -> Particle -> Particle
phiParticle dt pps original = _
  where
    -- changeIt (vx, vy) = dt * ()
    n = length filteredPPS
    leftN = length leftPPS
    rightN = n - leftN
    leftPPS = filter (leftOf (pos original) (vel original)) filteredPPS
    filteredPPS = filter (nearTo (pos original)) (Vec.toList $ fmap pos pps)
    nearTo pos1 pos2 = (distance2 pos1 pos2) < r2
    leftOf (x1, y1) (vx, vy) (x2, y2) = let
      phi = atan2 vy vx
      theta = atan2 (y2 - y1) (x2 - x1)
      change = theta - phi
      in (0 < change) && (change < pi)

updateParticle :: Int -> PPS -> Float -> Particle -> Particle
updateParticle n pps dt = versionsParticle n . moveParticle dt . phiParticle dt pps

updatePPS :: Float -> PPS -> PPS
updatePPS dt pps = fmap (updateParticle 3 pps dt) pps

simulatePPS :: PPS -> IO ()
simulatePPS pps = Gloss.simulate window background 10 pps displayPPS (const updatePPS)

main :: IO ()
main = Gloss.display window background drawing


