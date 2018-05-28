module Main where 

{- Needed for event handling and rendering -}
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss

{- Needed to retrieve command line arguments -}
import System.Environment   

import Data.Char
import ModelCalcs

data State = State {colors :: [Color], elaspedTime :: Float, cty :: City, rc :: (Integer, Integer)}

-- Represents the initial state for rendering to the screen [Red, Blue]
initState :: City -> (Integer, Integer) -> State 
initState cty (r, c) = State [makeColorI 255 0  0 255, makeColorI 0  0 255 255] 0 cty (r, c)

-- The number of frames per second to render
fps:: Int 
fps = 60 

{- This window configuration information using the "InWindow" function to create the window.-}
window :: Display 
window = InWindow "Schelling’s Model" (1024,768) (10, 10)

{- This is an example of rendering a "Picture" to the screen. 
   I'm using functions that are part of the Graphics.Gloss.Picture module. 
   Here I'm making the rectangles of the State object . 
   
   I use the "pictures" function to compose both the rectangles (rect1 & rect2 & rect3) into a single 
   picture to be rendered to the screen. 
-}
render :: State -> IO Picture 
render state = let
    {- NOTE: I'm making an assumption here that there will always be two rect colors in the state 
       thus I can use (!!)-}
    title = scale 0.3 0.3.translate (-1350) 1100.text $ "Schelling's Model of Housing Segregation*"
    --rect1 = translate 0  0.color((colors state) !! 0) $ rectangleSolid 50 50 
    --rect2 = translate 100 0.color((colors state) !! 1) $ rectangleSolid 50 50
    rects = getRectanglesFromCity (cty state) state
    in 
        return $ pictures ([title] ++ rects)

{- The event handlers handles events coming from the user: 
   In this program we won't interact with the user. See "examples/updating" 
   to see interaction with the user. 
-}
eventHandler :: Event -> State -> IO State 
eventHandler event state = return state

{- The update loop function is used to update the current state of the application -}
updateLoop :: Float -> State -> IO State 
updateLoop deltaTime (State rectColors eTime cty (r,c)) = return (State rectColors eTime cty (r,c))
    {-eTime' = eTime + deltaTime 
    in 
        if eTime' > 1.0 
            then do 
                let rectColors' = (tail rectColors) ++ [head rectColors]
                return $ (State rectColors' 0 cty (r,c))  
        else return $ (State rectColors eTime' cty (r,c))-}

{- This shows an example of rendering a window to the screen using the render function above. 
   window -> The configuration information for the window 
   white'' -> Background color for the window 
   fps -> the number of frames to render (you can always set this to 60)
   initState -> The initial State configuration 
   render -> the function that renders the State 
   eventHanlder -> the function that handles events from the user 
   updateLoop -> the function that updates the State of the program 

-} 

-- get color from HomeownerType
getColorFromType :: HomeownerType -> State -> Color
getColorFromType t st = if t == O then white
                        else if t == R then (colors st)!!0
                        else (colors st)!!1

-- return a list of rectangles given a City
getRectanglesFromCity :: City -> State -> [Picture]
getRectanglesFromCity [] _ = []
getRectanglesFromCity (x:xs) st = let (r,c) = rc st
                                      rectH = 600.0 / (fromIntegral r :: Float)
                                      rectW = 600.0 / (fromIntegral c :: Float)
                                      rindex = fromIntegral (fst $ coor x) :: Float
                                      cindex = fromIntegral (snd $ coor x) :: Float
                                      rectColor = getColorFromType (homeowner x) st
                                      rectX = -250.0 + cindex*rectW
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

-- takes a list of String HomeownerType values and creates a City
makeCityBodyList :: Integer -> Integer -> [String] -> City
makeCityBodyList _ _ [] = []
makeCityBodyList i c (x:xs) = if x == "B" then [Home (i `div` c, mod i c) B 0.0] ++ makeCityBodyList (i+1) c xs
                              else if x == "R" then [Home (i `div` c, mod i c) R 0.0] ++ makeCityBodyList (i+1) c xs
                              else [Home (i `div` c, mod i c) O 0.0] ++ makeCityBodyList (i+1) c xs

-- takes a String List from input data and returns a City
getCityFromData :: [String] -> City
getCityFromData cityGrid = let c = read (cityGrid!!1) :: Integer
                               (x:y:xs) = cityGrid
                           in makeCityBodyList 0 c (flattenList xs)

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
               playIO window white fps (initState cty (r,c)) Main.render eventHandler updateLoop
           -- input grid size
           else do
               putStrLn "asdf."
               --playIO window white fps initState (Main.render cty) eventHandler updateLoop
       else do
           putStrLn "Invalid arguments. Exiting."
           return ()
