{-# LANGUAGE TypeApplications #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import PPS
import Simulator
import Defs

import Control.Monad.Random.Lazy
import qualified Graphics.Gloss as Gloss

-- Input the size of your display here if you're displaying fullscreen, this will later be improved to work right 

maxX :: Int
maxX = floor (1920/2 :: Float)

maxY :: Int
maxY = floor (1080/2 :: Float)

minX :: Int
minX = -maxX

minY :: Int
minY = -maxY

ourPPS :: (MonadRandom m) => m PPS
ourPPS = genThetas 0 (2 * pi) $ gridAt 10 30 Gloss.red theGrid
  where
    theGrid = [(x, y) | x <- [minX,minX+60..maxX], y <- [minY,minY+60..maxY]]

ourConf :: (MonadRandom m) => m Config
ourConf = flip fmap ourPPS $ \pps -> Config {
    initpps = pps,
    alph = pi,
    beta = 69 * 180 / pi, -- Cassie's contribution, the 69'otron
    radius2 = (60 ** 2),
    howManyVersions=50,
    confWindow = Gloss.InWindow "Nice Window" (maxX, maxY) (0, 0),
    backgroundColor = Gloss.black,
    particleColor = \n -> const $ case n of
        0 -> Gloss.red
        1 -> Gloss.blue
        2 -> Gloss.green
        3 -> Gloss.yellow
        4 -> Gloss.cyan
        _ -> Gloss.magenta
  }

main :: IO ()
main = do
  config <- evalRandIO ourConf
  simulatePPS config
