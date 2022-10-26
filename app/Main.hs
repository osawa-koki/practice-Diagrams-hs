import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Apollonian
import Data.Colour.Palette.BrewerSet


-- -- 単純な円を描写
-- -- $ stack exec -- practice-Diagrams-hs-exe -o ./fruits/myCircle.svg -w 400
-- -- https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html#your-first-diagram

-- myCircle :: Diagram B
-- myCircle = circle 1
-- main = mainWith myCircle


-- -- apollonianGasketを描写
-- -- $ stack exec -- practice-Diagrams-hs-exe -o ./fruits/apollonianGasket.svg -w 400
-- -- https://archives.haskell.org/projects.haskell.org/diagrams/gallery/Apollonian.html

-- example = (apollonianGasket 0.01 2 3 3)
-- main = mainWith (example :: Diagram B)


-- ひまわりを描写
-- $ stack exec -- practice-Diagrams-hs-exe -o ./fruits/sunflower.svg -w 400
-- https://archives.haskell.org/projects.haskell.org/diagrams/gallery/Sunflower.html


mkCoords :: Int -> [P2 Double]
mkCoords n =[coord (fromIntegral i) | i <- [1..n]]
  where
    coord m = p2 $ fromPolar (sqrt m) (2.4 * m)
    fromPolar r theta = (r * cos theta, r * sin theta)

floret :: Double -> Diagram B
floret r = circle 0.6 # lw none # fc (colors !! n)
  where
    n = floor (1.4 * sqrt r) `mod` 10
    colors = black : (reverse $ brewerSet YlOrBr 9)

sunflower :: Int -> Diagram B
sunflower n = frame 4 $ position $ zip (mkCoords n) (florets n)
  where
    florets m = [floret (sqrt (fromIntegral i)) | i <- [1..m]]

example :: Diagram B
example = frame 4 $ sunflower 2000 # bg black

main = mainWith (example :: Diagram B)

