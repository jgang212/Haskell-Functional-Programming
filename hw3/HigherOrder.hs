{- 
  File      :  HigherOrder.hs 
  Copyright : (c) Jack Gang, 04/17/18 
  Contains implementations of higher order functions: listRange, maxSum,
    dedupe, prefixes, and kSublist.
-}

{- HELPER FUNCTIONS -}

{- takes two lists and returns a list of corresponding pairs; discards elements 
    of longer list -}
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

{- returns the suffix of a list given the length of the suffix -}
drop' :: Int -> [a] -> [a]
drop' n lst | n <= 0 = lst
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

{- returns the last element of a given list -}
last' :: [a] -> a
last' (x:xs) = lastHelper x xs
    where lastHelper y [] = y
          lastHelper _ (y:ys) = lastHelper y ys

{- addLists : takes in two lists and combines them (implementation of "++") -}
addLists :: [a] -> [a] -> [a]
addLists [] ys = ys
addLists (x:xs) ys = x : addLists xs ys

{- MAIN FUNCTIONS -}

{- listRange : takes a Int list and a range that is represent by a Int tuple 
    and returns a Int list of the elements inside the input list within a given
    range (inclusive). The elements of the returned list must be in the same
    order they appeared in the input list. -}

listRange :: [Int] -> (Int,Int) -> [Int]
listRange lst (a,b) = filter (\x -> x >= a && x <= b) lst

{- maxSum : takes in a list of Int lists and returns the sum of all the numbers
    inside the list of Int lists. -}

maxSum :: [[Int]] -> Int 
maxSum [] = 0
maxSum (x:xs) = foldr (\y acc -> y + acc) 0 x + maxSum xs

{- dedupe : takes in two arguments: a function that represents a equivalence 
    relation, and a Int list. The function returns a list containing the same
    elements as the input list, but without any duplicates, where two elements
    are considered equal if applying the equivalence relation input function to
    them yields true. This function should only remove consecutive duplicate
    values from a list based on the equivalence relation function.-}

{- need to re-add last value since zip removes it -}
dedupe :: (Int -> Int -> Bool) -> [Int] -> [Int]
dedupe f lst = addLists (map (fst) . filter (\(a,b) -> not (f a b)) $ zip' lst (drop' 1 lst)) [last' lst]

{- prefixes : takes in a Int list and returns a list of all non-empty prefixes 
    of a list, ordered from shortest to longest. -}

{- TODO: need to remove empty prefix -}
    
prefixes :: [Int] -> [[Int]]
prefixes [] = [[]]
prefixes (x:xs) = [] : map (x:) (prefixes xs)

{- kSublist : takes in a Int list and a Int . The function returns the 
    contiguous sublist of length k whose elements have the largest sum. -}

kSublist :: [Int] -> Int -> [Int]
kSublist [] len = []
kSublist (x:xs) len = if len == foldr (\_ accum -> 1 + accum) 0 (x:xs) then (x:xs)                          
                      else [5]






