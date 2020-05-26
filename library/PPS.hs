module PPS (gridAt, genThetas) where

import Defs

import qualified Data.Vector as Vec
import qualified Graphics.Gloss as Gloss
import Control.Monad.Random.Class

gridAt :: Float -> Float  -> Gloss.Color -> [(Int, Int)] -> PPS
gridAt sz speed col xys = Vec.fromList $ fmap makeParticle xys
  where
    makeParticle (x, y) = Particle {
        size = sz,
        color = col,
        pos = (fromIntegral x, fromIntegral y),
        vel = (speed, 0),
        past = []
      }

genTheta :: (MonadRandom m) => Float -> Float -> Particle -> m Particle
genTheta thetaMin thetaMax original = do
  theta <- getRandomR (thetaMin, thetaMax)
  pure $ original {
    vel = (fst $ vel original, theta)
  }

genThetas :: (MonadRandom m) => Float -> Float -> PPS -> m PPS
genThetas thetaMin thetaMax = traverse  (genTheta thetaMin thetaMax)
