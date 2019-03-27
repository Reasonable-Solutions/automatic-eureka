{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import Control.Arrow
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Random as Random
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable (for_)
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import qualified Graphics.Rendering.Cairo as Cairo
import Linear.V2
import Linear.Vector as V
import qualified Numeric.Noise.Perlin as P
import Text.Printf

import Lib

data World = World
  { worldWidth :: Int
  , worldHeight :: Int
  , worldSeed :: Int
  , worldScale :: Double
  }

-- a---b
-- |   |
-- c---d
data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  } deriving (Eq, Ord)



type Generate a = Random.RandT Random.StdGen (ReaderT World Cairo.Render) a

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w,h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$>  Random.getRandomR (3, w `div` 2 - 3)
      <*> Random.getRandomR (3, h `div` 2 -3)
    pure $ v ^* 2
  pure
    . nub
    . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)


quadAddNoise :: Quad -> Generate Quad
quadAddNoise Quad {..} = do
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

  pure $ Quad
      (addNoise quadA)
      (addNoise quadB)
      (addNoise quadC)

darkGunMetal :: Double -> Cairo.Render ()
darkGunMetal = hsva 170 0.30 0.16


eggshell :: Double -> Cairo.Render ()
eggshell = hsva 71 0.13 0.96

renderClosedPath :: [V2 Double] -> Cairo.Render ()
renderClosedPath (V2 x y:vs) = do
  Cairo.newPath
  Cairo.moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in Cairo.lineTo x' y'
  Cairo.closePath
renderClosedPath [] = pure ()

renderQuad :: Quad -> Cairo.Render ()
renderQuad Quad {..} = renderClosedPath [quadA, quadB, quadC]


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
 where RGB {..} = hsv h s v

renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1
  cairo $ Cairo.setLineWidth 0.15
  quads <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads

  for_ noisyQuads $ \quad -> do
    strokeOrFill <- Random.weighted [(Cairo.fill, 0.5), (Cairo.stroke, 0.5)]
    color <- Random.uniform [teaGreen, vividTangerine, englishVermillion, darkGunMetal]

    cairo $ do
     renderQuad $ quad
     color 1 *> strokeOrFill

teaGreen :: Double -> Cairo.Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Cairo.Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Cairo.Render ()
englishVermillion = hsva 355 0.68 0.84

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
