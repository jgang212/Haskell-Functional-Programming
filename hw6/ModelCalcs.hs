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

calcOneSimScore :: City -> (Integer, Integer) -> HomeownerType -> Integer -> (Double, Double)
calcOneSimScore city (r,c) color rad = let dim = floor(sqrt (fromIntegral (length city)))
                                           neighborhood = map (findHomeByRC city) (makeNeighborhood (r,c) rad dim)
                                           similar = foldr (\x acc -> (if (homeowner x) == color then 1 else 0) + acc) 0 neighborhood
                                           occupied = foldr (\x acc -> (if (homeowner x) /= O then 1 else 0) + acc) 0 neighborhood
                                       in if color /= O then (similar, occupied)
                                          else (0, 1)

calcSimScores :: Int -> City -> Integer -> City
calcSimScores i c r = let homeCoor = coor (c!!i)
                          ratio = calcOneSimScore c homeCoor (homeowner (c!!i)) r
                          newX = Home homeCoor (homeowner (c!!i)) ((fst ratio)/(snd ratio))
                      in if (i+1) == (length c) then [newX]
                         else [newX] ++ calcSimScores (i+1) c r

getLocationThres :: City -> (Integer, Integer) -> [Home] -> HomeownerType -> Double -> Integer -> [Double]
getLocationThres c hc [] ht thres r = []
getLocationThres c hc (x:xs) ht thres r = let dim = floor(sqrt (fromIntegral (length c)))
                                              neighborhood = makeNeighborhood (coor x) r dim
                                              ratio = calcOneSimScore c (coor x) ht r
                                          in if elem hc neighborhood then
                                                 [(fst ratio)/(snd ratio) - thres]
                                                 ++ getLocationThres c hc xs ht thres r
                                             else [((fst ratio)+1)/((snd ratio)+1) - thres]
                                                  ++ getLocationThres c hc xs ht thres r

getMovingLocation :: Int -> Double -> [Double] -> [Home] -> (Integer, Integer)
getMovingLocation i lookFor [] open = (-1, -1)
getMovingLocation i lookFor (x:xs) open = if x == lookFor then coor (open!!i)
                                          else getMovingLocation (i+1) lookFor xs open

updateOpenList :: City -> [Home] -> (Integer, Integer) -> (Integer, Integer) -> [Home]
updateOpenList c initOpen closed open = filter (\x -> coor x /= closed) initOpen ++ [findHomeByRC c open]

moveLocations :: Int -> City -> (Integer, Integer) -> (Integer, Integer) -> City
moveLocations i c initCoor newCoor = let checkCoor = coor (c!!i)
                                     in if checkCoor == initCoor then
                                            [Home checkCoor O 0.0] ++ moveLocations (i+1) c initCoor newCoor
                                        else if checkCoor == newCoor then
                                            [Home checkCoor (homeowner (findHomeByRC c initCoor)) 0.0]
                                            ++ moveLocations (i+1) c initCoor newCoor
                                        else if (i+1) == (length c) then [findHomeByRC c checkCoor]
                                        else [findHomeByRC c checkCoor] ++ moveLocations (i+1) c initCoor newCoor

{-
stepOnce :: Int -> City -> [Home] -> Integer -> Double -> City
stepOnce i c open r thres = let h = c!!i
                                openThres = getLocationThres c (coor h) open (homeowner h) thres r
                            in if simScore h >= thres then [h] ++ stepOnce (i+1) c open r thres
                               else -}

testCity = [Home (0,0) R 0.0, Home (0,1) R 0.0, Home (0,2) O 0.0, Home (0,3) R 0.0, Home (0,4) R 0.0, 
            Home (1,0) O 0.0, Home (1,1) B 0.0, Home (1,2) B 0.0, Home (1,3) B 0.0, Home (1,4) O 0.0, 
            Home (2,0) R 0.0, Home (2,1) R 0.0, Home (2,2) R 0.0, Home (2,3) R 0.0, Home (2,4) B 0.0, 
            Home (3,0) B 0.0, Home (3,1) B 0.0, Home (3,2) B 0.0, Home (3,3) O 0.0, Home (3,4) B 0.0, 
            Home (4,0) B 0.0, Home (4,1) R 0.0, Home (4,2) R 0.0, Home (4,3) R 0.0, Home (4,4) O 0.0]

initialOpenList = filter (\x -> homeowner x == O) testCity