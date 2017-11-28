module Lib where

import Data.Random.Normal
import qualified Data.Colour.RGBSpace.HSL as C
import Data.Colour.RGBSpace
import qualified System.Random as R
import Data.List (zipWith4)
import GHC.Word (Word8)

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

frameSize = 64
frameSizeHalf = 0.5 * (fromIntegral frameSize)

drawSystem :: String -> BMSystem -> IO ()
drawSystem filename system = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      img = renderDrawing frameSize frameSize white $
            sequence_ [withTexture (uniformTexture colour) $ fill $ circle (V2 (realToFrac$frameSizeHalf + frameSizeHalf*x) (realToFrac$frameSizeHalf + frameSizeHalf*y)) (realToFrac$frameSizeHalf*r) | (Particle r (Vector2D x y) _ _ colour) <- particles system]

  writePng (filename ++ ".png") img

data Vector2D = Vector2D {x::Double, y::Double}
data Particle = Particle {radius::Double, position::Vector2D, velocity::Vector2D, acceleration::Vector2D, colour::PixelRGBA8}
type System = [Particle]

instance Num Vector2D where
  (Vector2D ax ay) + (Vector2D bx by) = Vector2D (ax+bx) (ay+by)
  (Vector2D ax ay) * (Vector2D bx by) = Vector2D (ax*bx) (ay*by)
  negate (Vector2D ax ay) = Vector2D (negate ax) (negate ay)
  abs = undefined
  signum = undefined
  fromInteger = undefined

(.*) :: Double -> Vector2D -> Vector2D
(.*) s (Vector2D x y) = Vector2D (s*x) (s*y)

projectedInside r (Vector2D x y) = Vector2D x' y'
 where
   x' = if x > 1 - r then 1 - r else if x < r - 1 then r - 1 else x
   y' = if y > 1 - r then 1 - r else if y < r - 1 then r - 1 else y

bounced (Particle r (Vector2D sx sy) (Vector2D vx vy) _  _) = Vector2D vx' vy'
 where
  vx' = if abs sx > 1 - r then negate vx else vx
  vy' = if abs sy > 1 - r then negate vy else vy

step :: Double -> Double -> Particle -> Particle
step c dt particle = let
  velocityBounced = bounced particle
  dampedAcceleration = acceleration particle + (negate c) .* (velocity particle)
  position' = (projectedInside (radius particle) (position particle)) + dt .* velocityBounced
  velocity' = velocityBounced + dt .* dampedAcceleration
  in particle {position = position', velocity = velocity'}

makeVectors (x:y:xs) = (Vector2D x y) : makeVectors xs

zeroVector = Vector2D 0 0
zeroParticle r c = Particle {position=zeroVector, velocity=zeroVector, acceleration=zeroVector, radius=r, colour=c}

data BMSystem = BMSystem {randoms::[Vector2D], particles::[Particle]}

stepSystem :: Double -> Double -> BMSystem -> BMSystem
stepSystem c dt system = BMSystem {randoms = drop numParticles (randoms system), particles = map (step c dt) particles'}
 where
   numParticles = length (particles system)
   particles' = zipWith (\r p -> p {acceleration = r}) (randoms system) (particles system)

{-
randomColours seed = f ((R.randomRs (0,360) (R.mkStdGen seed)) :: [Double])
 where f (h:hs) = let (RGB r g b) = C.hsl h 1 0.5 in PixelRGBA8 (round (255*r)) (round (255*g)) (round (255*b)) 200 : f hs
-}

randomColours seed = [PixelRGBA8 x x x 128 | x <- ((R.randomRs (0,1) (R.mkStdGen seed)) :: [Word8])]

clamp l h x
 | x < l = l
 | x > h = h
 | otherwise = x

randomParticles seed averageRadius sdRadius = zipWith4 (\r p c s -> p {radius = r, position = s, colour = c})
   (map (clamp 0.05 0.4) $ mkNormals' (averageRadius, sdRadius) seed)
   (repeat (zeroParticle 1 white))
   (randomColours seed)
   (makeVectors (R.randomRs (-1,1) (R.mkStdGen seed)))
 where white = PixelRGBA8 255 255 255 255

someFunc :: IO ()
someFunc = do
  let seed = 211
  let dt = 0.01
  let frames = 1000
  let numParticles = 15
  let averageRadius = 0.15
  let sdRadius = 0
  let sd = 10
  let viscosity = 1.2
  let rs = makeVectors (mkNormals' (0,sd) seed)
  let startSystem = BMSystem rs (take numParticles (randomParticles seed averageRadius sdRadius))
  let systems = drop 5 $ iterate (stepSystem viscosity dt) startSystem
  let fileNames = ["mov/frame " ++ show n | n <- [1..]]
  sequence_ $ zipWith drawSystem fileNames (take frames systems)
