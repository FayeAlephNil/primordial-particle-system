module Defs where

import qualified Graphics.Gloss as Gloss
import qualified Data.Vector as Vec

type Velocity = Gloss.Vector
type Position = Gloss.Point

data GenConfig = GenConfig
  {
    speed :: Float,
    initcolor :: Gloss.Color
  }

data Config = Config
  {
    initpps :: PPS,
    alph :: Float,
    beta :: Float,
    radius2 :: Float,
    howManyVersions :: Int,
    confWindow :: Gloss.Display,
    backgroundColor :: Gloss.Color,
    particleColor :: Int -> Particle -> Gloss.Color
  }

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
changePos f p = p { pos = f (pos p) }

changeVel :: (Velocity -> Velocity) -> Particle -> Particle
changeVel f p = p { vel = f (vel p) }
                     
setPast :: Maybe Particle -> Particle -> Particle
setPast newPast p = p { past = newPast }

setColor :: Gloss.Color -> Particle -> Particle
setColor newCol p = p { color = newCol }

particleToList :: Maybe Particle -> [Particle]
particleToList Nothing = []
particleToList (Just p) = p : (particleToList (past p))

listToParticle :: [Particle] -> Maybe Particle
listToParticle [] = Nothing
listToParticle (p : ps) = Just $ setPast (listToParticle ps) p
