{- 
  File      :  ImageEffects.hs 
  Copyright : (c) Jack Gang, 05/09/18 
  Contains data types and image effect functions for PPM images.
-}

module ImageEffects
(
   PPMImage(..),
   Pixel(..),
   getValuesFromPixel,
   negateR,
   negateG,
   negateB,
   greyScale
) where

data PPMImage a = PPMImage {width :: Integer, 
    height:: Integer,
    magicNumber :: Integer, 
    maxColor :: Integer, 
    pixels :: [a]}
    deriving (Show)

newtype Pixel a = Pixel (a,a,a) deriving (Show)

--newtype Kernel = Kernel [Integer] deriving (Show)

getValuesFromPixel :: Pixel a -> [a]
getValuesFromPixel (Pixel (x, y, z)) = [x, y, z]

instance Functor PPMImage where
    fmap f (PPMImage w h mn mc p) = PPMImage w h mn mc (map f p)

negateR :: (Integral a) => a -> Pixel a -> Pixel a
negateR mc (Pixel (r, g, b)) = Pixel (mc-r, g, b)

negateG :: (Integral a) => a -> Pixel a -> Pixel a
negateG mc (Pixel (r, g, b)) = Pixel (r, mc-g, b)

negateB :: (Integral a) => a -> Pixel a -> Pixel a
negateB mc (Pixel (r, g, b)) = Pixel (r, g, mc-b)

greyScale :: (Integral a) => Pixel a  -> Pixel a
greyScale (Pixel (r, g, b)) = let avg = (r + g + b) `div` 3
                              in Pixel (avg, avg, avg)

--sharpenKernel = Kernel [0, -1, 0, -1, 5, -1, 0, -1, 0]

testImage1 = PPMImage 2 2 3 255 [(Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255))]