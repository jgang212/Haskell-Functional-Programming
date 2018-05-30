{- 
  File      :  ModelCalcs.hs 
  Copyright : (c) Jack Gang, 06/06/18 
  Contains data types and simulation functions for Schelling's model.
-}

module ModelCalcs
(
    HomeownerType(..),
    Home(..),
    City(..),
    calcSimScores,
    simulate',
    isCitySatisfied,
    indexCity,
    getCityFromRandom
) where

{----------------- DATA TYPES -----------------}

-- B = blue, R = red, O = open
data HomeownerType = B | R | O
                     deriving (Show, Eq)

-- a Home has a coordinate, type, and a score (defaulted to 0.0)
data Home = Home {coor :: (Integer, Integer), homeowner :: HomeownerType, simScore :: Double}
            deriving (Show)

-- a City is just a list of Homes
type City = [Home]

{----------------- HELPER FUNCTIONS -----------------}

-- returns a Home in a City given its coordinates
findHomeByRC :: City -> (Integer, Integer) -> Home
findHomeByRC city (r,c) = let dim = floor(sqrt (fromIntegral (length city)))
                          in (city)!!(fromInteger (r*dim + c))

-- make a Neighborhood given the center coordinate, radius, and bounds
makeNeighborhood :: (Integer, Integer) -> Integer -> Integer -> [(Integer, Integer)]
makeNeighborhood (r,c) 0 lim = [(r,c)]
makeNeighborhood (r,c) rad lim = let minR = min (max (r - rad) 0) (lim-1)
                                     minC = min (max (c - rad) 0) (lim-1)
                                     maxR = min (max (r + rad) 0) (lim-1)
                                     maxC = min (max (c + rad) 0) (lim-1)
                                 in [(x,y) | x <- [minR..maxR], y <- [minC..maxC]]

-- inputs: City, coordinates of Home, home type, radius
-- for a single Home in a City, calculate the similarity score given a home type and radius
-- returns the value as a fraction tuple (not the actual double)
calcOneSimScore :: City -> (Integer, Integer) -> HomeownerType -> Integer -> (Double, Double)
calcOneSimScore city (r,c) color rad = let dim = floor(sqrt (fromIntegral (length city)))
                                           neighborhood = map (findHomeByRC city) (makeNeighborhood (r,c) rad dim)
                                           similar = foldr (\x acc -> (if (homeowner x) == color then 1 else 0) + acc) 0 neighborhood
                                           occupied = foldr (\x acc -> (if (homeowner x) /= O then 1 else 0) + acc) 0 neighborhood
                                       in if color /= O then (similar, occupied)
                                          else (0, 1)   -- simScore = 0 for open homes

-- inputs: City, coordinates of Home, list of open homes, home type, similarity threshold, radius
-- when evaluating a single Home in a city to potentially move, generate this list of similarity scores of
-- all of the open homes it is considering moving to
getLocationThres :: City -> (Integer, Integer) -> [Home] -> HomeownerType -> Double -> Integer -> [Double]
getLocationThres c hc [] ht thres r = []
getLocationThres c hc (x:xs) ht thres r = let dim = floor(sqrt (fromIntegral (length c)))
                                              neighborhood = makeNeighborhood (coor x) r dim
                                              ratio = calcOneSimScore c (coor x) ht r
                                             -- if new location is also in neighborhood, simScore doesn't change
                                          in if elem hc neighborhood then
                                                 [(fst ratio)/(snd ratio) - thres]
                                                 ++ getLocationThres c hc xs ht thres r
                                             -- if new location is not in neighborhood, simScore changes
                                             else [((fst ratio)+1)/((snd ratio)+1) - thres]
                                                  ++ getLocationThres c hc xs ht thres r

-- get the "best" open location to move to given a list of threshold diffs for the open homes
getBestThreshold :: [Double] -> Double
getBestThreshold thresDiffs = let positiveOpens = filter (\x -> x > 0) thresDiffs
                              in if (length positiveOpens) > 0 then minimum positiveOpens
                                 else -9.9    -- -9.9 will never be a value in threshold diffs for open homes

-- inputs: index (0 start), threshold looking for, all possible thresholds, list of open homes
-- gets the coordinates of the moving location given the threshold we're looking for and a list of thresholds
-- always returns first one it finds (been on the market the longest)
getMovingLocation :: Int -> Double -> [Double] -> [Home] -> (Integer, Integer)
getMovingLocation i lookFor [] open = (-1, -1)
getMovingLocation i lookFor (x:xs) open = if x == lookFor then coor (open!!i)
                                          else getMovingLocation (i+1) lookFor xs open

-- inputs: City, initial open homes list, previous occupied home location, moved to location
-- update the open homes list given a City and coordinates of the old and new homes
updateOpenList :: City -> [Home] -> (Integer, Integer) -> (Integer, Integer) -> [Home]
updateOpenList c initOpen closed open = filter (\x -> coor x /= closed) initOpen ++ [findHomeByRC c open]

-- inputs: index (0 start), City, initial location, new location
-- return a new City after a move, given the move coordinates and the initial City
moveLocations :: Int -> City -> (Integer, Integer) -> (Integer, Integer) -> City
moveLocations _ c _ (-1, -1) = c
moveLocations i c initCoor newCoor = let checkCoor = coor (c!!i)
                                        -- go through and check if a Home is either the initial or new locations, otherwise do nothing
                                     in if checkCoor == initCoor then
                                            if (i+1) == (length c) then [Home checkCoor O 0.0]
                                            else [Home checkCoor O 0.0] ++ moveLocations (i+1) c initCoor newCoor
                                        else if checkCoor == newCoor then
                                            if (i+1) == (length c) then [Home checkCoor (homeowner (findHomeByRC c initCoor)) 0.0]
                                            else 
                                                [Home checkCoor (homeowner (findHomeByRC c initCoor)) 0.0]
                                                ++ moveLocations (i+1) c initCoor newCoor
                                        else if (i+1) == (length c) then [findHomeByRC c checkCoor]
                                        else [findHomeByRC c checkCoor] ++ moveLocations (i+1) c initCoor newCoor

-- inputs: index (0 start), City, list of open homes, radius, satisfaction threshold
-- within one simulation step, this function is a single "step" of evaluating a single home's potential move
-- returns both the resulting City and the new list of open homes in a tuple
stepOnce :: Int -> City -> [Home] -> Integer -> Double -> (City, [Home])
stepOnce i c open r thres = let h = c!!i
                                openThres = getLocationThres c (coor h) open (homeowner h) thres r
                                bestThres = getBestThreshold openThres
                                movingLocation = getMovingLocation 0 bestThres openThres open
                                newCity = calcSimScores 0 (moveLocations 0 c (coor h) movingLocation) r                     
                            in if (simScore h >= thres) || (homeowner h == O) then (c, open)
                               else 
                                   if movingLocation /= (-1,-1) then
                                        (newCity, updateOpenList c open movingLocation (coor h))
                                   else (c, open)

{----------------- SMHS FUNCTIONS -----------------}

-- inputs: index (0 start), City, radius
-- takes in a City and (re)calculates the simulation score values for each home
calcSimScores :: Int -> City -> Integer -> City
calcSimScores i c r = let homeCoor = coor (c!!i)
                          ratio = calcOneSimScore c homeCoor (homeowner (c!!i)) r
                          newX = Home homeCoor (homeowner (c!!i)) ((fst ratio)/(snd ratio))  -- same home but with simScore
                      in if (i+1) == (length c) then [newX]   -- termination
                         else [newX] ++ calcSimScores (i+1) c r

-- inputs: index (0 start), City, list of open homes, radius, satisfaction threshold
-- simulates one step of the SMHS and returns both the resulting City and the new list of open homes in a tuple
simulate' :: Int -> City -> [Home] -> Integer -> Double -> (City, [Home])
simulate' i c open r thres = let (city, openHomes) = stepOnce i c open r thres
                             in if (i+1) == (length c) then (city, openHomes)   -- termination
                                else simulate' (i+1) city openHomes r thres

-- takes in a City and a satisfaction threshold and returns whether or not the entire City is satisfied
isCitySatisfied :: City -> Double -> Bool
isCitySatisfied [] _ = True     -- return True if all passes
isCitySatisfied (x:xs) thres = if (homeowner x) == O || (simScore x) >= thres then isCitySatisfied xs thres
                               else False

-- inputs: City, dimension of City, index (0 start)
-- takes in a City and reindexes it (used after shuffling)
indexCity :: City -> Integer -> Int -> City
indexCity [] _ _ = []
indexCity (x:xs) size i = let Home rowcol htype sim = x
                          in [Home (toInteger (i `div` (fromIntegral size)), toInteger (mod i (fromIntegral size))) htype sim] ++ indexCity xs size (i+1)

-- inputs: dimension of City, red percent, blue percent, empty percent
-- returns an unshuffled City with given red/blue/empty percents and size
getCityFromRandom :: Integer -> Integer -> Integer -> Integer -> City
getCityFromRandom size r b e = -- convert percentages to counts
                               let emptyNum = ceiling ((fromIntegral e :: Float) / 100 * (fromIntegral size :: Float))
                                   remaining = size - emptyNum
                                   red = ceiling ((fromIntegral r :: Float) / 100 * (fromIntegral remaining :: Float))
                                   blue = remaining - red
                               in (replicate (fromIntegral emptyNum) (Home (0,0) O 0.0)) ++ 
                                  (replicate (fromIntegral red) (Home (0,0) R 0.0)) ++ 
                                  (replicate (fromIntegral blue) (Home (0,0) B 0.0))