{-# LANGUAGE TypeApplications #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import PPS
import Simulator
import Defs

import Control.Monad.Random.Lazy
import qualified Graphics.Gloss as Gloss
import Options.Applicative

-- Input the size of your display here if you're displaying fullscreen, this will later be improved to work right

data WindowOptions = WindowOpts {screenx :: Int, screeny :: Int}
data ParticleOptions = ParticleOptions { spd :: Float, sizeit :: Float }
data PPSType = Grid (Int, Int) deriving (Read, Show, Eq)
data SimOptions = SimOptions {simspeed :: Float, simalpha :: Float, simbeta :: Float, versionCount :: Int, rad :: Float}

data CmdOptions = CmdOptions
  { windowOpts :: WindowOptions,
    particleOpts :: ParticleOptions,
    ppstype :: PPSType,
    simOpts :: SimOptions
  }

makePPS :: (MonadRandom m) => CmdOptions -> m PPS
makePPS copts  = goForIt (ppstype copts)
  where
    wopts = windowOpts copts
    popts = particleOpts copts
    maxX = quot (screenx wopts) 2
    maxY = quot (screeny wopts) 2
    minX = -maxX
    minY = -maxY
    goForIt :: (MonadRandom m) => PPSType -> m PPS
    goForIt (Grid (spacex, spacey))= let
      theGrid = [(x, y) | x <- [minX+5,minX+spacex..maxX-5], y <- [minY+5,minY+spacey..maxY-5]]
      in genThetas 0 (2 * pi) $ gridAt (sizeit popts) (spd popts) Gloss.red theGrid

windowParser :: Parser WindowOptions
windowParser = WindowOpts
               <$> option auto
                   (long "screenx"
                    <> short 'x'
                    <> help "Size of the screen in the x direction"
                    <> value 1980
                    <> metavar "INT")
               <*> option auto
                   (long "screeny"
                    <> short 'y'
                    <> help "Size of the screen in the y direction"
                    <> value 1080
                    <> metavar "INT")

particleParser :: Parser ParticleOptions
particleParser = ParticleOptions
                 <$> option auto
                     (long "speed"
                      <> short 's'
                      <> help "Speed of the particles"
                      <> value 30
                      <> metavar "FLOAT")
                 <*> option auto
                     (long "size"
                      <> short 'z'
                      <> help "Size of the particles"
                      <> value 10
                      <> metavar "FLOAT")

ppsParser :: Parser PPSType
ppsParser = option auto
            (long "pps"
             <> short 'p'
             <> help "The Type of PPS"
             <> value (Grid (50,50)))

simParser :: Parser SimOptions
simParser = SimOptions
            <$> option auto
                (long "simspeed"
                <> help "Multiplier on the rate of the simulation"
                <> value 1
                <> metavar "FLOAT")
            <*> option auto
                (long "alpha"
                <> short 'a'
                <> help "Alpha for the constant turn"
                <> value pi
                <> metavar "FLOAT")
            <*> option auto
                (long "beta"
                <> short 'b'
                <> help "Beta multiplier in the differential equation"
                <> value (69 * 180/pi) -- Thanks Cassie
                <> metavar "FLOAT")
            <*> option auto
                (long "paths"
                <> help "How many positions to keep around in our paths"
                <> value 50
                <> metavar "FLOAT")
            <*> option auto
                (long "radius"
                <> short 'r'
                <> help "The radius a particle searches in for the differential equation"
                <> value 60
                <> metavar "FLOAT")

cmdParser :: Parser CmdOptions
cmdParser = CmdOptions <$> windowParser <*> particleParser <*> ppsParser <*> simParser

makeConf :: (MonadRandom m) => CmdOptions -> m Config
makeConf copts = flip fmap (makePPS copts) $ \pps -> Config {
    initpps = pps,
    alph = simalpha . simOpts $ copts,
    beta = simbeta . simOpts $  copts, -- 69 * 180 / pi, -- Cassie's contribution, the 69'otron
    radius2 = (rad. simOpts $ copts) ** 2, -- (60 ** 2),
    howManyVersions= versionCount . simOpts $ copts,
    confWindow = Gloss.InWindow "Nice Window" (maxX, maxY) (0, 0),
    backgroundColor = Gloss.black,
    particleColor = \n -> const $ case n of
        -1 -> Gloss.red
        0 -> Gloss.red
        1 -> Gloss.blue
        2 -> Gloss.green
        3 -> Gloss.yellow
        4 -> Gloss.cyan
        _ -> Gloss.magenta,
    speedUp = simspeed . simOpts $ copts
  }
  where
    wopts = windowOpts copts
    maxX = quot (screenx wopts) 2
    maxY = quot (screeny wopts) 2

main :: IO ()
main = withOptions =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
           (fullDesc
            <> progDesc "Simulate Primordial Particle Systems")

withOptions :: CmdOptions -> IO ()
withOptions copts = evalRandIO (makeConf copts) >>= simulatePPS
