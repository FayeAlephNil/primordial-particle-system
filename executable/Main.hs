-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import PPS
import Simulator
import Defs

import Control.Monad.Random.Lazy
import qualified Graphics.Gloss as Gloss

maxX :: Int
maxX = 1920

maxY :: Int
maxY = 1080

ourPPS :: (MonadRandom m) => m PPS
ourPPS = genThetas 0 (2 * pi) $ gridAt 10 30 Gloss.red theGrid
  where
    theGrid = [(x, y) | x <- [0,60..maxX], y <- [0,60..maxY]]

ourConf :: (MonadRandom m) => m Config
ourConf = flip fmap ourPPS $ \pps -> Config {
    initpps = pps,
    alph = pi,
    beta = 3 * pi,
    radius2 = 4 * (60 ** 2),
    howManyVersions=50,
    confWindow = Gloss.InWindow "Nice Window" (maxX, maxY) (0, 0),
    backgroundColor = Gloss.black,
    particleColor = const $ const Gloss.red
  }

main :: IO ()
main = do
  config <- evalRandIO ourConf
  simulatePPS config
