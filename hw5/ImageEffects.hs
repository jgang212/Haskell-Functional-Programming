{- 
  File      :  ImageEffects.hs 
  Copyright : (c) Jack Gang, 05/15/18 
  Contains data types and image effect functions for PPM images.
-}

module ImageEffects
(
   PPMImage(..),
   Pixel(..),
   FixedPPMImage(..),
   getValuesFromPixel,
   negateR,
   negateG,
   negateB,
   greyScale,
   convolution
) where

{----------------- DATA TYPES -----------------}

data PPMImage a = PPMImage {width :: Integer, 
    height:: Integer,
    magicNumber :: Integer, 
    maxColor :: Integer, 
    pixels :: [a]}
    deriving (Show)

newtype Pixel a = Pixel (a,a,a) deriving (Show)

instance Functor PPMImage where
    fmap f (PPMImage w h mn mc p) = PPMImage w h mn mc (map f p)

-- Kernel is just a list of Integers (assume length = 9)
type Kernel = [Integer]

newtype FixedPPMImage = FixedPPMImage {getImg:: PPMImage (Pixel Integer)} deriving (Show)

-- mempty is a 10x10 black picture, since we want 0 values for our "Sum" function
instance Monoid (FixedPPMImage) where
  mempty = FixedPPMImage (PPMImage 10 10 3 255 (replicate 100 (Pixel (0, 0, 0))))
  x `mappend` y = FixedPPMImage (PPMImage 10 10 3 255 (addPixelLists (pixels $ getImg x) (pixels $ getImg y)))

{----------------- HELPER FUNCTIONS -----------------}

-- constrains an Integer Pixel value between 0 and maxColor
constrainPixelValue :: Integer -> Integer -> Integer
constrainPixelValue val mc = if val > mc then mc
                             else if val < 0 then 0
                             else val

-- Within a PPMImage, find a particular Pixel by its row and column indices
findPixelByRC :: (Integral a) => PPMImage (Pixel a) -> Integer -> Integer -> Pixel a
findPixelByRC ppm r c = let h = height ppm
                            w = width ppm
                        in if (r < 0) || (c < 0) || (r+1 > h) || (c+1 > w) then Pixel (0, 0, 0)
                           else (pixels ppm)!!(fromInteger (r*w + c))

-- get R, G, B values, respectively from a Pixel
getR :: Pixel a -> a
getR (Pixel (x, y, z)) = x

getG :: Pixel a -> a
getG (Pixel (x, y, z)) = y

getB :: Pixel a -> a
getB (Pixel (x, y, z)) = z

-- performs convolution on a given Pixel indicated by row and column indices within a PPMImage, using a given Kernel
-- the "f" function parameter is either getR, getG, or getB to indicate which value is convoluted
convolutionSingle :: PPMImage (Pixel Integer) -> Kernel -> Integer -> Integer -> (Pixel Integer -> Integer) -> Integer
convolutionSingle ppm k r c f = let mc = maxColor ppm
                              -- multiply 9 values in Kernel by 9 values around and including pixel
                              in constrainPixelValue (f (findPixelByRC ppm (r-1) (c-1)) * (k!!0) +
                                 f (findPixelByRC ppm (r-1) (c)) * (k!!1) +
                                 f (findPixelByRC ppm (r-1) (c+1)) * (k!!2) +
                                 f (findPixelByRC ppm (r) (c-1)) * (k!!3) +
                                 f (findPixelByRC ppm (r) (c)) * (k!!4) +
                                 f (findPixelByRC ppm (r) (c+1)) * (k!!5) +
                                 f (findPixelByRC ppm (r+1) (c-1)) * (k!!6) +
                                 f (findPixelByRC ppm (r+1) (c)) * (k!!7) +
                                 f (findPixelByRC ppm (r+1) (c+1)) * (k!!8)) mc

-- sum the values of two lists of Pixels
addPixelLists :: [Pixel Integer] -> [Pixel Integer] -> [Pixel Integer]
addPixelLists [] [] = []
addPixelLists (x:xs) (y:ys) = [Pixel (getR x + getR y, getG x + getG y, getB x + getB y)] ++ addPixelLists xs ys

{----------------- IMAGE EFFECT FUNCTIONS -----------------}

-- return the values of a Pixel in list format for output purposes
getValuesFromPixel :: Pixel a -> [a]
getValuesFromPixel (Pixel (x, y, z)) = [x, y, z]

-- negate-red
negateR :: (Integral a) => a -> Pixel a -> Pixel a
negateR mc (Pixel (r, g, b)) = Pixel (mc-r, g, b)

-- negate-green
negateG :: (Integral a) => a -> Pixel a -> Pixel a
negateG mc (Pixel (r, g, b)) = Pixel (r, mc-g, b)

-- negate-blue
negateB :: (Integral a) => a -> Pixel a -> Pixel a
negateB mc (Pixel (r, g, b)) = Pixel (r, g, mc-b)

-- grey-scale
greyScale :: (Integral a) => Pixel a  -> Pixel a
greyScale (Pixel (r, g, b)) = let avg = (r + g + b) `div` 3
                              in Pixel (avg, avg, avg)

-- convolution given a PPMImage, a Kernel, and a starting row and column index (should be 0 and 0)
-- convolutes each Pixel in an image, from left-to-right, top-to-bottom
convolution :: PPMImage (Pixel Integer) -> Kernel -> Integer -> Integer -> [Pixel Integer]
convolution ppm k startRow startCol = [Pixel (convolutionSingle ppm k startRow startCol getR,
                                             convolutionSingle ppm k startRow startCol getG,
                                             convolutionSingle ppm k startRow startCol getB)]
                                      ++ if (startCol+1) >= (width ppm) && (startRow+1) >= (height ppm) then []     -- stop recursion when run out of pixels
                                         else if (startCol+1) >= (width ppm) then convolution ppm k (startRow+1) 0  -- go to the next row
                                         else convolution ppm k startRow (startCol+1)                               -- go to the next column