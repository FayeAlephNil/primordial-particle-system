-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Graphics.Gloss as Gloss

window :: Gloss.Display
window = Gloss.InWindow "Nice Window" (200, 200) (10, 10)

background :: Gloss.Color
background = Gloss.black

main :: IO ()
main = return ()
