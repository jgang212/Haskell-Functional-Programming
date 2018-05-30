{- 
  File      :  smhs.hs 
  Copyright : (c) Jack Gang, 06/06/18 
  Contains the main IO functions for Schelling's model.
-}

module Main where 

{- Needed for event handling and rendering -}
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss

{- Needed to retrieve command line arguments -}
import System.Environment

import Shuffle
import Data.Char    -- for isDigit function
import ModelCalcs

{----------------- DATA TYPES -----------------}

-- the state of the output
data State = State {colors :: [Color], 
                    cty :: City,                -- current city
                    initCty :: City,            -- initial city
                    rc :: (Integer, Integer),   -- # rows and # cols for City
                    maxSteps :: Integer,        -- max # of steps allowed
                    threshold :: Double,        -- satisfaction threshold
                    radius :: Integer,          -- neighborhood radius
                    curRound :: Integer}        -- current round

{----------------- HELPER FUNCTIONS -----------------}

-- get color from HomeownerType
getColorFromType :: HomeownerType -> State -> Color
getColorFromType t st = if t == O then white
                        else if t == R then (colors st)!!0
                        else (colors st)!!1

-- return a list of rectangles (with coordinates) given a City
getRectanglesFromCity :: City -> State -> [Picture]
getRectanglesFromCity [] _ = []
getRectanglesFromCity (x:xs) st = let (r,c) = rc st
                                      rectH = 500.0 / (fromIntegral r :: Float)
                                      rectW = 500.0 / (fromIntegral c :: Float)
                                      rindex = fromIntegral (fst $ coor x) :: Float
                                      cindex = fromIntegral (snd $ coor x) :: Float
                                      rectColor = getColorFromType (homeowner x) st
                                      rectX = -215.0 + cindex*rectW
                                      rectY = 250.0 - rindex*rectH
                                 in [translate rectX rectY.color(rectColor) $ rectangleSolid rectW rectH]
                                    ++ [translate rectX rectY.color(black) $ rectangleWire rectW rectH]
                                    ++ getRectanglesFromCity xs st

-- check if a String is numerical
isStrNumber :: String -> Bool
isStrNumber ""  = True
isStrNumber (x:xs) = (isDigit x || x == ' ') && (isStrNumber xs)

-- check if arguments are valid before doing anything else
checkIfArgsValid :: [String] -> Bool
checkIfArgsValid args = if (length args /= 5) then False
                         else if (isStrNumber (args!!0) == False || isStrNumber (args!!2) == False || 
                                  isStrNumber (args!!3) == False || isStrNumber (args!!4) == False) then False
                         else True

-- flatten list of Strings for easier input from grid file
flattenList :: [String] -> [String]
flattenList [] = []
flattenList (x:xs) = words x ++ flattenList xs

-- takes a list of String HomeownerType values and creates an unindexed City
makeCityBodyList :: Integer -> [String] -> City
makeCityBodyList _ [] = []
makeCityBodyList i (x:xs) = if x == "B" then [Home (0,0) B 0.0] ++ makeCityBodyList (i+1) xs
                              else if x == "R" then [Home (0,0) R 0.0] ++ makeCityBodyList (i+1) xs
                              else [Home (0,0) O 0.0] ++ makeCityBodyList (i+1) xs

-- takes a String List from input data and returns a City
getCityFromData :: [String] -> City
getCityFromData cityGrid = let c = read (cityGrid!!1) :: Integer
                               (x:y:xs) = cityGrid
                           in indexCity (makeCityBodyList 0 (flattenList xs)) c 0

-- check if an integer is between 0-100
checkValidRange :: Integer -> Bool
checkValidRange num = if (num > 100) || (num < 0) then False
                      else True

{----------------- IO FUNCTIONS -----------------}

-- Represents the initial state for rendering to the screen
-- colors are [Red, Blue]
-- calculate the simScores on the City before initializing the state with the City
-- default values: threshold = 0.44, radius = 1, curRound = 0
initState :: City -> (Integer, Integer) -> Integer -> State 
initState cty (r, c) ms = let simCity = calcSimScores 0 cty 1
                          in State [makeColorI 255 0  0 255, makeColorI 0  0 255 255] simCity cty (r, c) ms 0.44 1 0

-- The number of frames per second to render
fps:: Int 
fps = 60 

-- This window configuration information uses the "InWindow" function to create the window
window :: Display 
window = InWindow "Schelling’s Model" (1024,1024) (10, 10)

-- rendering a "Picture" to the screen
render :: State -> IO Picture 
render state = let
    -- need these values for calculations/display below
    totalHomes = fromIntegral (length (filter (\x -> (homeowner x) /= O) (cty state))) :: Float
    redHomes = fromIntegral (length (filter (\x -> (homeowner x) == R) (cty state))) :: Float
    blueHomes = fromIntegral (length (filter (\x -> (homeowner x) == B) (cty state))) :: Float
    satHomes = fromIntegral (length (filter (\x -> (simScore x) >= (threshold state)) (cty state))) :: Float
    rects = getRectanglesFromCity (cty state) state
    -- display status of the simulation on the bottom right
    status = if (curRound state) == 0 then "Start"
             else if isCitySatisfied (cty state) (threshold state) then "Satisifed"
             else if (curRound state) < (maxSteps state) then "Stepping"
             else "Out of Steps"

    -- title and information below grid
    title = scale 0.3 0.3.translate (-1350) 1100.text $ "Schelling's Model of Housing Segregation"    
    round = scale 0.1 0.1.translate (-2500) (-2600).text $ "Round " ++ (show $ curRound state) ++ " of " ++ (show $ maxSteps state)
    satis = scale 0.1 0.1.translate (-2500) (-2750).text $ "Satisfied: " ++ (show $ 100 * (satHomes / totalHomes)) ++ "%"
    rad = scale 0.15 0.15.translate (-1700) (-2150).text $ "R-size: " ++ (show $ radius state)
    thres = scale 0.15 0.15.translate (-1700) (-2300).text $ "Similar: " ++ (show $ (100 * (threshold state))) ++ "%"
    percs = scale 0.15 0.15.translate (-1700) (-2450).text $ "Red/Blue: " ++ (show $ 100 * (redHomes / totalHomes)) ++ "%/"
            ++ (show $ 100.0 * (blueHomes / totalHomes)) ++ "%"
    empty = scale 0.15 0.15.translate (-1700) (-2600).text $ "Empty: " ++ (show $ 100 * (1 - totalHomes / fromIntegral (length (cty state)) :: Float)) ++ "%"
    sz = scale 0.15 0.15.translate (-1700) (-2750).text $ "Size: " ++ (show (fst $ rc state)) ++ "x" ++ (show (snd $ rc state))
    current = scale 0.15 0.15.translate (1300) (-1900).text $ "Status: " ++ status
    in 
        return $ pictures ([title, round, satis, rad, thres, percs, empty, sz, current] ++ rects)

-- The event handlers handles events coming from the user
eventHandler :: Event -> State -> IO State 

-- This pattern matches for Key Events
eventHandler (EventKey (Char key) Up _ _) state@(State col city initCity rowcol ms thres rad rnd) = let
        openHomes = filter (\x -> homeowner x == O) city
        in case key of 
        's' -> if rnd < ms then 
                   return $ (State col (fst (simulate' 0 city openHomes rad thres)) initCity rowcol ms thres rad (rnd+1))
               else return state
        'r' -> return (initState initCity rowcol ms)
        otherwise  -> return state 

-- This pattern matches for Special key Events
eventHandler (EventKey (SpecialKey key) Up _ _) state@(State col city initCity rowcol ms thres rad rnd) = case key of  
        -- Handles the events for the arrow keys 
        KeyUp    -> if (thres + 0.05) > 1 then return state
                    else return $ (State col city initCity rowcol ms (thres+0.05) rad rnd)
        KeyDown  -> if (thres - 0.05) < 0 then return state
                    else return $ (State col city initCity rowcol ms (thres-0.05) rad rnd)
        KeyRight  -> if rnd > 0 then return state
                    else return $ (State col city initCity rowcol ms thres (rad+1) rnd)
        KeyLeft -> if rnd > 0 then return state
                    else if rad <= 1 then return state
                    else return $ (State col city initCity rowcol ms thres (rad-1) rnd)

-- The catch all pattern
eventHandler _ state = return state  

-- The update loop function is used to update the current state of the application
-- not used
updateLoop :: Float -> State -> IO State 
updateLoop deltaTime st = return st

-- The main function for displaying a window  
main :: IO() 
main = do
       args <- getArgs
       if checkIfArgsValid args then do
           -- input grid file
           if (isStrNumber(args!!1) == False) then do
               inputData <- readFile (args!!1)
               let inputStrings = lines inputData
               let r = read (inputStrings!!0) :: Integer
               let c = read (inputStrings!!1) :: Integer
               let cty = getCityFromData inputStrings
               let maxSteps = read (args!!0) :: Integer
               playIO window white fps (initState cty (r,c) maxSteps) Main.render eventHandler updateLoop
               
           -- input grid size
           else do
               let r = read (args!!1) :: Integer
               let c = read (args!!1) :: Integer
               let rPerc = read (args!!2) :: Integer
               let bPerc = read (args!!3) :: Integer
               let ePerc = read (args!!4) :: Integer
               if (checkValidRange rPerc) && (checkValidRange bPerc) && (checkValidRange ePerc) then do
                   let cty = getCityFromRandom (r*r) rPerc bPerc ePerc
                   shuffledCty <- shuffle cty
                   let indexedCty = indexCity shuffledCty r 0
                   let maxSteps = read (args!!0) :: Integer
                   playIO window white fps (initState indexedCty (r,c) maxSteps) Main.render eventHandler updateLoop
               else do putStrLn "Invalid percentage values. Exiting."
       else do
           putStrLn "Invalid arguments. Exiting."
           return ()
