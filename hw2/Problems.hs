{- Write a function called "numberOf"
      numberOf :: Int -> [Int] -> Int 
   
   The function returns the number of times the first argument is inside the list. 

    *Main> numberOf 1 [1,2,3,1]
    2
    *Main> numberOf 12 [1,2,3,1]
    0

-}

numberOf :: Int -> [Int] -> Int 
numberOf find [] = 0
numberOf find (x:xs)
    | find == x = 1 + (numberOf find xs)
    | otherwise = numberOf find xs

{- Write a function called "contains"
      contains :: Int -> [Int] -> Bool 
 
   The function returns True if the first argument is contained within the list, otherwise False 

    *Main> contains 2 [1,2,3,4]
    True
    *Main> contains 23 [1,2,3,4]
    False

-}

contains :: Int -> [Int] -> Bool 
contains find [] = False
contains find (x:xs)
    | find == x = True
    | otherwise = contains find xs

{- Write a function called "duplicates"
      duplicates :: [Int] -> Bool 

   The function returns True if there are duplicates within the list, otherwise False. 

   *Main> duplicates [1,2,3,1,2]
   True

   *Main> duplicates [100,200,300]
   False

-}

duplicates :: [Int] -> Bool
duplicates [] = False
duplicates (x:xs)
    | contains x xs == True = True
    | otherwise = duplicates xs


{- Write a function called "isPalindrome"


   A palindrome is a word that reads the same forwards and backwards, like "level" or "sees" or "deified".   
   Don't worry about upper and lowercase issues, assume everything is lowercase. 
   
   Hint: "=="" can be used on lists. Try it out in the ghci 

   "ABCDEFGFEDCBA" is a palindrome
   "ABCDEFGGFEDCBA: is a palindrome
   "ABCDEFGEDCBA" is not a palindrome
   
-}

{- reverse' : takes in a list and simply put, reverses the elements in the 
    list. (You cannot use the Prelude's reverse function in your solution.) -}
    
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:[]) = [x]
reverse' (x:xs) = reverse xs ++ [x]

isPalindrome :: String -> Bool 
isPalindrome [] = True 
isPalindrome (x:[]) = True 
isPalindrome xs 
    | xs == (reverse xs) = True  -- Note: You should define your own reverse function. I did not because it's been assigned for your homework. :-) 
    | otherwise = False 
