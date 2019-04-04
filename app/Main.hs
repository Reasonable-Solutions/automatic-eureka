{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import Debug.Trace
import Control.Arrow
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Random as Random
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable (for_)
import Data.List (nub, nubBy)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import qualified Graphics.Rendering.Cairo as Cairo
import Linear.V2
import Linear.V3
import Linear.Vector as V
import Linear.Matrix as M
import Linear.Matrix ()
import qualified Numeric.Noise.Perlin as P
import Text.Printf
import Data.Function (on)

import Lib

data World = World
  { worldWidth :: Int
  , worldHeight :: Int
  , worldSeed :: Int
  , worldScale :: Double
  }

backgroundColor :: Double -> Cairo.Render ()
backgroundColor = hsva 42 0.109 0.97

shapeColor1 :: Double -> Cairo.Render ()
shapeColor1 = hsva 6 0.545 1

shapeColor2 :: Double -> Cairo.Render ()
shapeColor2 = hsva 38 0.57 0.976

shapeColor3 :: Double -> Cairo.Render ()
shapeColor3 = hsva  30 0.163 0.816

-- a---b
-- |   |
-- d---c
class Shapely a where
  toList :: a -> [V2 Double]

instance Shapely Shape where
  toList (Sq Square {..}) = [squareA, squareB, squareC, squareD]
  toList (Be Bend {..}) = [lA, lB, lC, lD, lE, lF]
  toList (Ci (Circle r (V2 x y))) = [V2 ((x + 0.75) + (r * cos theta)) ((y + 0.75) + (r * sin theta)) | theta <- [0, 0.01..2*pi] ]

data Bend = Bend
  { lA :: V2 Double
  , lB :: V2 Double
  , lC :: V2 Double
  , lD :: V2 Double
  , lE :: V2 Double
  , lF :: V2 Double
  } deriving (Eq, Ord)

data Square = Square
  { squareA :: V2 Double
  , squareB :: V2 Double
  , squareC :: V2 Double
  , squareD :: V2 Double
  } deriving (Eq, Ord)

data Circle = Circle { r :: Double, v :: V2 Double } deriving (Eq, Ord)

data Shape = Sq Square | Be Bend | Ci Circle deriving (Eq)

data ShapeType = S | L | C

type Generate a = Random.RandT Random.StdGen (ReaderT World Cairo.Render) a

translateM :: V2 Double -> V3 (V3 Double)
translateM (V2 dx dy) =
  let
    dir' = V3 dx dy 1.0
    translation (V3 x' y' _) = V3 (V3 1.0 0 x') (V3 0 1 y') (V3 0 0 1)
  in translation dir'

rotation r = V3 (V3 (cos r) (-sin r) 0) (V3 (sin r) (cos r) 0) (V3 0 0 1)

-- this needs to be over a list of V2 to alist of V2
rotateShape :: Double -> V2 Double -> V2 Double
rotateShape rad v@(V2 x y) =
  let
    v' = v
  in  (\((V3 x y _)) -> [(V2 x y)])
  $ translateM v'
  !*! rotation rad
  !*! translateM (negated v') !* (V3 x y 1)

-- >>> rotateShape (pi) (V2 1.5 1.5)
-- V2 3.0 3.0

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

makeShape :: ShapeType -> V2 Double -> Shape
makeShape S v = Sq $ Square v (v ^+^ V2 0 1.5) (v ^+^ V2 1.5 1.5) (v ^+^ V2 1.5 0)
makeShape L v =
  Be $ Bend
  v
  (v ^+^ V2 1.5 0)
  (v ^+^ V2 1.5 1.5)
  (v ^+^ V2 0.75 1.5)
  (v ^+^ V2 0.75 0.75)
  (v ^+^ V2 0 0.75)
makeShape C v = Ci $ Circle 0.75 v

genTripGrid :: Generate [Shape]
genTripGrid = do
  (w,h) <- getSize @Int
  shapes <- replicateM 800 $ do
    shape <- Random.weighted [(S, 1), (L, 1), (C, 1)]
    v <- V2 <$>  Random.getRandomR (3, w `div` 2 - 3)
      <*> Random.getRandomR (3, h `div` 2 -3)
    pure (v ^* 2, shape)
  pure $ flip map (nubBy ((==) `on` fst) shapes) $ \(v,s) ->
    let v' = fromIntegralVector v
    in makeShape s v'


shapeAddNoise :: Shape -> Generate [V2 Double]
shapeAddNoise shape = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistence = 0.5
    perlinNoise =
      P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistence
    perlin2d (V2 x y) =
      P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v =
      let
        noise = perlin2d v
      in
        v ^+^ V2 (noise / 100) (noise / 8)

  pure $ case shape of
    Sq sh -> addNoise <$> toList shape
    Be sh -> addNoise <$> (toList shape)
    Ci sh -> addNoise <$> toList shape

renderClosedPath :: [V2 Double] -> Cairo.Render ()
renderClosedPath (V2 x y:vs) = do
  Cairo.newPath
  Cairo.moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in Cairo.lineTo x' y'
  Cairo.closePath
renderClosedPath [] = pure ()

renderShape :: [V2 Double] -> Cairo.Render ()
renderShape = renderClosedPath

-- | Lift a Cairo into a generate action
cairo :: Cairo.Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a,a)
getSize = do
  (w,h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

fillScreen :: (Double -> Cairo.Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w,h) <- getSize @Double
  cairo $ do
    Cairo.rectangle 0 0 w h
    color opacity *> Cairo.fill

hsva :: Double -> Double -> Double -> (Double -> Cairo.Render ())
hsva h s v = Cairo.setSourceRGBA channelRed channelGreen channelBlue
 where RGB channelRed channelGreen channelBlue = hsv h s v

renderSketch :: Generate ()
renderSketch = do
  fillScreen backgroundColor 1
  cairo $ Cairo.setLineWidth 0.15
  trips <- genTripGrid
  noisyTrips <- traverse shapeAddNoise trips

  for_ noisyTrips $ \trip -> do
    strokeOrFill <- Random.weighted [(Cairo.fill, 0.5), (Cairo.stroke, 0.5)]
    color <- Random.uniform [shapeColor1, shapeColor2, shapeColor3]

    cairo $ do
     renderShape trip
     color 1 *> strokeOrFill

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = Random.mkStdGen seed
    width = 60
    height = 60
    scaleAmount = 20

    scaleWidth = round $ fromIntegral width * scaleAmount
    scaleHeight = round $ fromIntegral height * scaleAmount

  surface <- Cairo.createImageSurface Cairo.FormatARGB32 scaleWidth scaleHeight

  let world = World width height seed scaleAmount

  void
    . Cairo.renderWith surface
    . flip runReaderT world
    . flip Random.runRandT stdGen
    $ do
      cairo $ Cairo.scale scaleAmount scaleAmount
      renderSketch

  putStrLn "Generating art"
  Cairo.surfaceWriteToPNG surface
    $ "images/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  Cairo.surfaceWriteToPNG surface "images/latest.png"
