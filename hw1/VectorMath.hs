{- 
  File      :  VectorMath.hs 
  Copyright : (c) Jack Gang, 04/03/18 
  Contains a type alias for representing two-component vectors and functions 
  working on them. 
-}

{- type alias for representing a 2D Vector as a tuple -}
type Vec2 = (Double, Double)


{- scale : This function returns a Vec2 with its components equal to the 
original vector but scaled (i.e., multiplied) by the scalar argument. -}
scale :: Vec2 -> Double -> Vec2 
scale vec mult = (fst vec * mult,  snd vec * mult)


{- dot : This function performs a type of multiplication (product) on vectors.
The returned value is a scalar real number. The dot product of two vectors is
computed as follows. (x1, y1) * (x2, y2) = x1 * x2 + y1 * y2 -}
dot :: Vec2 -> Vec2 -> Double
dot vec1 vec2 = fst vec1 * fst vec2 + snd vec1 * snd vec2


{- vec2Length : The length of a vector (i.e., its magnitude) is computed from
its components using the Pythagorean theorem. -}
vec2Length :: Vec2 -> Double
vec2Length vec = sqrt $ fst vec ^ 2 + snd vec ^ 2


{- normalize : The function creates (and returns) a new vector by normalizing
the input vector. This means that the resulting vector has the same direction
but a magnitude of 1. In short, the new vector is the original vector scaled by
the inverse of its length. -}
normalize :: Vec2 -> Vec2
normalize vec = let vecLen = vec2Length vec     --Assume vecLen is not zero
                        in (fst vec / vecLen, snd vec / vecLen)
                        
                        
{- Checks to see if two vectors are equal to each other -}
checkVector :: Vec2 -> Vec2 -> String 
checkVector gotVec expectVec = printMessage where 
          printMessage = if eqChk  then  "Test Passed" 
                         else  "Test Failed, Got: " ++  (show gotVec) ++ 
                               ", Expected: "  ++ (show expectVec)    
          eqChk =  (fst gotVec == fst expectVec) && 
                   (snd gotVec == snd expectVec)


{- Checks to see if two doubles are equal to each other -}
checkDouble :: Double -> Double -> String
checkDouble got expected = printMessage where 
          printMessage = if got == expected  then  "Test Passed" 
                         else   "Test Failed, Got: " ++ show got ++ 
                                         ", Expected: "  ++ show expected


{- Holds a list of test cases for the vector functions -}
{- TODO: You need to write test functions using the above functions...  -}
testResults :: [String]
testResults = let test1 = checkVector (scale (1.0,1.0) 2) (2.0, 2.0)
                  test2 = checkDouble 1.5 1.5 
              in  [test1,test2]