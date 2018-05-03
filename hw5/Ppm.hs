import Data.Char
import ImageEffects

setInputAndOutput :: IO (String, String)
setInputAndOutput = do 
    putStrLn "Enter in an input PPM file:"
    inputStr <- getLine
    putStrLn "Enter in an output PPM file:"
    outputStr <- getLine
    return (inputStr, outputStr) 

imageLoop :: PPMImage a -> String -> IO ()
imageLoop ppm output = do
    decision <- mainMenu
    if decision == 1 then do
        putStrLn "Reg-negated input PPM image."
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
        let ppm = PPMImage 2 2 3 255 [(Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255)),
                              (Pixel (255,0,255))]
        imageLoop ppm outputFile