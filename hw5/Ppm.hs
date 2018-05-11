import Data.Char
import ImageEffects

setInputAndOutput :: IO (String, String)
setInputAndOutput = do 
    putStrLn "Enter in an input PPM file:"
    inputStr <- getLine
    putStrLn "Enter in an output PPM file:"
    outputStr <- getLine
    return (inputStr, outputStr) 

blendOrApply :: IO Int
blendOrApply = do 
    putStrLn "============== Main Menu =============="
    putStrLn "(1) Apply image effects"
    putStrLn "(2) Blend distinct images"
    putStr  "Option: \n"
    optStr <- getLine 
    let optionInt = read optStr :: Int 
    return optionInt

imageLoop :: PPMImage (Pixel Integer) -> String -> IO ()
imageLoop ppm output = do
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
    else if decision == 5 then do
        let kernel = [-1, -1, -1, -1, 8, -1, -1, -1, -1]
            newPPM = PPMImage (width ppm) (height ppm) (magicNumber ppm) (maxColor ppm) (convolution ppm kernel 0 0)
        putStrLn "Edge detection-ed input PPM image."
        imageLoop newPPM output
    else if decision == 6 then do
        let kernel = [0, -1, 0, -1, 5, -1, 0, -1, 0]
            newPPM = PPMImage (width ppm) (height ppm) (magicNumber ppm) (maxColor ppm) (convolution ppm kernel 0 0)
        putStrLn "Sharpened input PPM image."
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
        putStrLn "Invalid choice. Please try again."
        imageLoop ppm output

blendLoop :: FixedPPMImage -> IO ()
blendLoop initialFPPM = do
    putStrLn "Enter in a Fixed PPM to blend:"
    inputStr <- getLine
    if inputStr == "q" then do
        let ppm = getImg initialFPPM
        writeFile "blended.ppm" $ ("P" ++ (show (magicNumber ppm)) ++ "\n" ++ 
                                  (show (width ppm)) ++ " " ++ (show (height ppm)) ++ "\n" ++
                                  (show (maxColor ppm)) ++ "\n" ++
                                  interspersePixelSpace (getOutputFromPixels (pixels ppm)))
        putStrLn "Saved current PPM to blended.ppm file."
        main
    else do
        inputData <- readFile inputStr
        let inputStrings = lines inputData
        let ppm = getPPMFromData inputStrings
        let fppm = FixedPPMImage (getImg $ (FixedPPMImage ppm) `mappend` initialFPPM)
        blendLoop fppm

mainMenu :: IO Int  
mainMenu = do 
    putStrLn "============== Apply Actions =============="
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

-- pixelLineToPixels :: [String] -> [Pixel Integer]
-- pixelLineToPixels [] = []
-- pixelLineToPixels [x] = []
-- pixelLineToPixels [x, _] = []
-- pixelLineToPixels (x:y:z:xs) = [Pixel (read x :: Integer, read y :: Integer, read z :: Integer)] ++ pixelLineToPixels xs

flattenList :: [String] -> [String]
flattenList [] = []
flattenList (x:xs) = words x ++ flattenList xs

makeImageBodyList :: [String] -> [Pixel Integer]
makeImageBodyList [] = []
makeImageBodyList (x:y:z:xs) = [Pixel (read x :: Integer, read y :: Integer, read z :: Integer)] ++ makeImageBodyList xs

getPPMFromData :: [String] -> PPMImage (Pixel Integer)
getPPMFromData ppmData = let mn = toInteger (digitToInt (ppmData!!0!!1))
                             w = read ((words (ppmData!!1))!!0) :: Integer
                             h = read ((words (ppmData!!1))!!1) :: Integer
                             mc = read (ppmData!!2) :: Integer
                             (x:y:z:xs) = ppmData
                             body = makeImageBodyList (flattenList xs)
                         in PPMImage w h mn mc body

getOutputFromPixels :: [Pixel Integer] -> [Integer]
getOutputFromPixels [] = []
getOutputFromPixels (x:xs) = getValuesFromPixel x ++ getOutputFromPixels xs

interspersePixelSpace :: [Integer] -> String
interspersePixelSpace [] = []
interspersePixelSpace (x:xs) = (show x) ++ " \n" ++ (interspersePixelSpace xs)

main :: IO () 
main = do
    mainDecision <- blendOrApply
    if mainDecision == 1 then do
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
    else if mainDecision == 2 then do
        let fppm = FixedPPMImage (PPMImage 10 10 3 255 (replicate 100 (Pixel (0, 0, 0))))
        blendLoop fppm
    else do
        putStrLn "Invalid choice. Please try again."
        main