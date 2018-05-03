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

getValuesFromPixel :: Pixel a -> [a]
getValuesFromPixel (Pixel (x, y, z)) = [x, y, z]

instance Functor PPMImage where
    fmap f (PPMImage w h mn mc p) = PPMImage w h mn mc (map f p)

negateR :: (Integral a) => Pixel a  -> Pixel a
negateR (Pixel (r, g, b)) = Pixel (255-r, g, b)

negateG :: (Integral a) => Pixel a  -> Pixel a
negateG (Pixel (r, g, b)) = Pixel (r, 255-g, b)

negateB :: (Integral a) => Pixel a  -> Pixel a
negateB (Pixel (r, g, b)) = Pixel (r, g, 255-b)

greyScale :: (Integral a) => Pixel a  -> Pixel a
greyScale (Pixel (r, g, b)) = let avg = (r + g + b) `div` 3
                              in Pixel (avg, avg, avg)

testImage1 = PPMImage 2 2 3 255 [(Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255))]