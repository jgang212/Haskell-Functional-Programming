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
   greyScale,
   convolution
) where

data PPMImage a = PPMImage {width :: Integer, 
    height:: Integer,
    magicNumber :: Integer, 
    maxColor :: Integer, 
    pixels :: [a]}
    deriving (Show)

newtype Pixel a = Pixel (a,a,a) deriving (Show)

type Kernel = [Integer]

constrainPixelValue :: Integer -> Integer -> Integer
constrainPixelValue val mc = if val > mc then mc
                             else if val < 0 then 0
                             else val

findPixelByRC :: (Integral a) => PPMImage (Pixel a) -> Integer -> Integer -> Pixel a
findPixelByRC ppm r c = let h = height ppm
                            w = width ppm
                        in if (r < 0) || (c < 0) || (r+1 > h) || (c+1 > w) then Pixel (0, 0, 0)
                           else (pixels ppm)!!(fromInteger (r*w + c))

getValuesFromPixel :: Pixel a -> [a]
getValuesFromPixel (Pixel (x, y, z)) = [x, y, z]

getR :: Pixel a -> a
getR (Pixel (x, y, z)) = x

getG :: Pixel a -> a
getG (Pixel (x, y, z)) = y

getB :: Pixel a -> a
getB (Pixel (x, y, z)) = z

convolutionSingle :: PPMImage (Pixel Integer) -> Kernel -> Integer -> Integer -> (Pixel Integer -> Integer) -> Integer
convolutionSingle ppm k r c f = let mc = maxColor ppm
                              in constrainPixelValue (f (findPixelByRC ppm (r-1) (c-1)) * (k!!0) +
                                 f (findPixelByRC ppm (r-1) (c)) * (k!!1) +
                                 f (findPixelByRC ppm (r-1) (c+1)) * (k!!2) +
                                 f (findPixelByRC ppm (r) (c-1)) * (k!!3) +
                                 f (findPixelByRC ppm (r) (c)) * (k!!4) +
                                 f (findPixelByRC ppm (r) (c+1)) * (k!!5) +
                                 f (findPixelByRC ppm (r+1) (c-1)) * (k!!6) +
                                 f (findPixelByRC ppm (r+1) (c)) * (k!!7) +
                                 f (findPixelByRC ppm (r+1) (c+1)) * (k!!8)) mc

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

convolution :: PPMImage (Pixel Integer) -> Kernel -> Integer -> Integer -> [Pixel Integer]
convolution ppm k startRow startCol = [Pixel (convolutionSingle ppm k startRow startCol getR,
                                             convolutionSingle ppm k startRow startCol getG,
                                             convolutionSingle ppm k startRow startCol getB)]
                                      ++ if (startCol+1) >= (width ppm) && (startRow+1) >= (height ppm) then []
                                         else if (startCol+1) >= (width ppm) then convolution ppm k (startRow+1) 0
                                         else convolution ppm k startRow (startCol+1)

sharpenKernel = [0, -1, 0, -1, 5, -1, 0, -1, 0]

testImage1 = PPMImage 3 3 3 255 [(Pixel (1,1,1)), (Pixel (2,2,2)), (Pixel (3,3,3)),
                                 (Pixel (4,4,4)), (Pixel (5,5,5)), (Pixel (6,6,6)),
                                 (Pixel (7,7,7)), (Pixel (8,8,8)), (Pixel (9,9,9))]