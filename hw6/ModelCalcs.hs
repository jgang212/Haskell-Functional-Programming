{- 
  File      :  ModelCalcs.hs 
  Copyright : (c) Jack Gang, 06/06/18 
  Contains data types and simulation functions for Schelling's model.
-}

{----------------- DATA TYPES -----------------}

data HomeownerType = B | R | O
                     deriving (Show, Eq)

data Home = Home {coor :: (Integer, Integer), homeowner :: HomeownerType, simScore :: Double}
            deriving (Show)

type City = [Home]

findHomeByRC :: City -> (Integer, Integer) -> Home
findHomeByRC city (r,c) = let dim = floor(sqrt (fromIntegral (length city)))
                         in (city)!!(fromInteger (r*dim + c))

makeNeighborhood :: (Integer, Integer) -> Integer -> Integer -> [(Integer, Integer)]
makeNeighborhood (r,c) 0 lim = [(r,c)]
makeNeighborhood (r,c) rad lim = let minR = min (max (r - rad) 0) (lim-1)
                                     minC = min (max (c - rad) 0) (lim-1)
                                     maxR = min (max (r + rad) 0) (lim-1)
                                     maxC = min (max (c + rad) 0) (lim-1)
                                 in [(x,y) | x <- [minR..maxR], y <- [minC..maxC]]

calcOneSimScore :: City -> (Integer, Integer) -> Integer -> Double
calcOneSimScore city (r,c) rad = let dim = floor(sqrt (fromIntegral (length city)))
                                     neighborhood = map (findHomeByRC city) (makeNeighborhood (r,c) rad dim)
                                     color = homeowner (findHomeByRC city (r,c))
                                     similar = foldr (\x acc -> (if (homeowner x) == color then 1 else 0) + acc) 0 neighborhood
                                     occupied = foldr (\x acc -> (if (homeowner x) /= O then 1 else 0) + acc) 0 neighborhood
                                 in if color /= O then similar / occupied
                                    else 0.0

calcSimScores :: Int -> City -> Integer -> City
calcSimScores i c r = let homeCoor = coor (c!!i)
                          newX = Home homeCoor (homeowner (c!!i)) (calcOneSimScore c homeCoor r)
                      in if (i+1) == (length c) then [newX]
                         else [newX] ++ calcSimScores (i+1) c r

testCity = [Home (0,0) R 0.0, Home (0,1) R 0.0, Home (0,2) O 0.0, Home (0,3) R 0.0, Home (0,4) R 0.0, 
            Home (1,0) O 0.0, Home (1,1) B 0.0, Home (1,2) B 0.0, Home (1,3) B 0.0, Home (1,4) O 0.0, 
            Home (2,0) R 0.0, Home (2,1) R 0.0, Home (2,2) R 0.0, Home (2,3) R 0.0, Home (2,4) B 0.0, 
            Home (3,0) B 0.0, Home (3,1) B 0.0, Home (3,2) B 0.0, Home (3,3) O 0.0, Home (3,4) B 0.0, 
            Home (4,0) B 0.0, Home (4,1) R 0.0, Home (4,2) R 0.0, Home (4,3) R 0.0, Home (4,4) O 0.0]