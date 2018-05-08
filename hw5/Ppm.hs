import Data.Char
import ImageEffects

setInputAndOutput :: IO (String, String)
setInputAndOutput = do 
    putStrLn "Enter in an input PPM file:"
    inputStr <- getLine
    putStrLn "Enter in an output PPM file:"
    outputStr <- getLine
    return (inputStr, outputStr) 

imageLoop :: PPMImage (Pixel Integer) -> String -> IO ()
imageLoop ppm output = do
    putStrLn (show ppm)
    decision <- mainMenu
    if decision == 1 then do
        let newPPM = fmap (negateR (maxColor ppm)) $ ppm
        putStrLn "Red-negated input PPM image."
        imageLoop newPPM output
    else if decision == 2 then do
        let newPPM = fmap (negateG (maxColor ppm)) $ ppm
        putStrLn "Green-negated input PPM image."
        imageLoop newPPM output
    else if decision == 3 then do
        let newPPM = fmap (negateB (maxColor ppm)) $ ppm
        putStrLn "Blue-negated input PPM image."
        imageLoop newPPM output
    else if decision == 4 then do
        let newPPM = fmap greyScale $ ppm
        putStrLn "Grey-scaled input PPM image."
        imageLoop newPPM output
    else if decision == 7 then do
        writeFile output $ ("P" ++ (show (magicNumber ppm)) ++ "\n" ++ 
                            (show (width ppm)) ++ " " ++ (show (height ppm)) ++ "\n" ++
                            (show (maxColor ppm)) ++ "\n" ++
                            interspersePixelSpace (getOutputFromPixels (pixels ppm)))
        putStrLn "Saved current PPM to output file."
        imageLoop ppm output
    else if decision == 8 then do
        return ()
    else do
        putStrLn "asdf"

mainMenu :: IO Int  
mainMenu = do 
    putStrLn "============== Actions =============="
    putStrLn "(1) Red-negate"
    putStrLn "(2) Green-negate"
    putStrLn "(3) Blue-negate"
    putStrLn "(4) Grey-scale"
    putStrLn "(5) Edge detection"
    putStrLn "(6) Sharpen"
    putStrLn "(7) Save to output file"
    putStrLn "(8) Exit"
    putStr  "Option: \n"
    optStr <- getLine 
    let optionInt = read optStr :: Int 
    return optionInt

checkIfInputValid :: [String] -> Bool
checkIfInputValid header = if (header!!0 /= "P3") then False
                           else if (isStrNumber (header!!1) == False || length (header!!1) == 0) then False
                           else if (isStrNumber (header!!2) == False || length (header!!2) == 0) then False
                           else True

isStrNumber :: String -> Bool
isStrNumber ""  = True
isStrNumber (x:xs) = (isDigit x || x == ' ') && (isStrNumber xs)

pixelLineToPixels :: [String] -> [Pixel Integer]
pixelLineToPixels [] = []
pixelLineToPixels [x] = []
pixelLineToPixels [x, _] = []
pixelLineToPixels (x:y:z:xs) = [Pixel (read x :: Integer, read y :: Integer, read z :: Integer)] ++ pixelLineToPixels xs

makeImageBodyList :: [String] -> [Pixel Integer]
makeImageBodyList [] = []
makeImageBodyList (x:xs) = let dataLine = words x
                           in pixelLineToPixels dataLine ++ makeImageBodyList xs

getPPMFromData :: [String] -> PPMImage (Pixel Integer)
getPPMFromData ppmData = let mn = toInteger (digitToInt (ppmData!!0!!1))
                             w = read ((words (ppmData!!1))!!0) :: Integer
                             h = read ((words (ppmData!!1))!!1) :: Integer
                             mc = read (ppmData!!2) :: Integer
                             body = makeImageBodyList ppmData
                         in PPMImage w h mn mc body

getOutputFromPixels :: [Pixel Integer] -> [Integer]
getOutputFromPixels [] = []
getOutputFromPixels (x:xs) = getValuesFromPixel x ++ getOutputFromPixels xs

interspersePixelSpace :: [Integer] -> String
interspersePixelSpace [] = []
interspersePixelSpace (x:xs) = (show x) ++ " " ++ (interspersePixelSpace xs)

main :: IO () 
main = do     
    files <- setInputAndOutput
    inputData <- readFile (fst files)
    let inputStrings = lines inputData
    if checkIfInputValid inputStrings == False then do
        putStrLn "Invalid Input File"
        return ()
    else do
        let outputFile = snd files
        let ppm = getPPMFromData inputStrings
        imageLoop ppm outputFile