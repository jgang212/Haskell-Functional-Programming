{- 
  File      :  Recursion.hs 
  Copyright : (c) Jack Gang, 04/10/18 
  Contains functions to practice list recursion and functional patterns, as
  well as smaller helper functions.
-}

module Recursion
( intersperse
, weave
, pairSwap
, reverse'
, unzip'
, zip'
) where

{--------------------------------------------------------}

{- Data Types -}

data PointTy = Point2D | Point3D | Point4D 
  deriving (Show, Eq)
data Point a = Point PointTy [a]
  deriving (Show, Eq)
  
{--------------------------------------------------------}

{- Helper functions -}

{- addLists : takes in two lists and combines them (implementation of "++") -}
addLists :: [a] -> [a] -> [a]
addLists [] ys = ys
addLists (x:xs) ys = x : addLists xs ys

{- getType: get the PointTy of a point -}
getType :: Point a -> PointTy
getType (Point pt _) = pt

{- getCoor: get the coordinates of a point -}
getCoor :: Point a -> [a]
getCoor (Point _ a) = a

{- length': get the length of a list -}
length' :: [a] -> Integer
length' [] = 0                         
length' (_:xs) = 1 + length' xs

{- getDistance: returns the distance between two 2D points -}
getDistance :: Point Double -> Point Double -> Double
getDistance a b = let aCoor = getCoor a
                      bCoor = getCoor b
                  in sqrt $ (aCoor!!0 - bCoor!!0) ^ 2 + (aCoor!!1 - bCoor!!1) ^ 2

{- isPythagoreanTrip: returns True if the set of distances form a Pythagorean 
    triple -}
isPythagoreanTrip :: [Double] -> Bool
isPythagoreanTrip a = sqrt((a!!0) ^ 2 + (a!!1) ^ 2) == a!!2
                  
{- isRightTri: return whether a set of 3 2D points forms a right triangle -}
isRightTri :: [Point Double] -> Bool
isRightTri a = let dist1 = getDistance (a!!0) (a!!1)
                   dist2 = getDistance (a!!0) (a!!2)
                   dist3 = getDistance (a!!1) (a!!2)
               in 
               if dist1 < dist2 then
                   if dist2 < dist3 then isPythagoreanTrip [dist1, dist2, dist3]
                   else if dist1 < dist3 then isPythagoreanTrip [dist1, dist3, dist2]
                   else isPythagoreanTrip [dist3, dist1, dist2]
               else
                   if dist1 < dist3 then isPythagoreanTrip [dist2, dist1, dist3]
                   else if dist2 < dist3 then isPythagoreanTrip [dist2, dist3, dist1]
                   else isPythagoreanTrip [dist3, dist2, dist1]
                  
{- findRightTrisHelper: helper function for findRindTris that uses a counter to
    keep track of indices -}
findRightTrisHelper :: [[Point Double]] -> Integer -> [Integer]
findRightTrisHelper [] _ = []
findRightTrisHelper (x:xs) c = 
    if (length' (findPoint Point2D x) == 3) && (isValid x) then
        if isRightTri x then
            addLists [c] (findRightTrisHelper xs (c+1))
        else findRightTrisHelper xs (c+1)
    else
        findRightTrisHelper xs (c+1)

{--------------------------------------------------------}

{- Main functions -}

{- intersperse : takes in a list and a value and returns a list where the value
    appears before and after every element in the list. If the input list is 
    empty then the result is a list only containing the value. -}
    
intersperse :: [a] -> a -> [a]
intersperse [] a = [a]
intersperse (x:xs) a = addLists [a,x] (intersperse xs a)

{- weave : takes in two lists and weaves the elements of lists together. If 
    either list is exhausted before the other (i.e., the lists do not contain
    the same number of elements), then add the remaining elements of the non-
    exhausted list to the returned list. -}
    
weave :: [a] -> [a] -> [a]
weave [] ys = ys
weave xs [] = xs
weave (x:xs) (y:ys) = addLists [x,y] (weave xs ys)

{- pairSwap : takes a list of pairs and returns a list of pairs. The returned
    list matches the input list except that the elements of each pair have had
    their positions swapped. -}
    
pairSwap :: [(a,b)] -> [(b,a)]
pairSwap [] = []
pairSwap (x:xs) = addLists [(snd x, fst x)] (pairSwap xs)

{- reverse' : takes in a list and simply put, reverses the elements in the 
    list. (You cannot use the Prelude's reverse function in your solution.) -}
    
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = addLists (reverse xs) [x]

{- unzip' : takes a list of pairs and returns a pair of lists. The returned 
    pair of lists contains a list of the first elements of the input pair ( a )
    and a list of the second elements of the input pairs ( b ). (You cannot use
    the Prelude's unzip in your solution.)-}

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' (x:xs) = let (a, b) = x
                in (a:(fst other), b:(snd other))
                where other = unzip' xs
                
{- zip' : takes two input lists of a and b and returns a list of ( a , b ) 
    pairs. The returned list is constructed by pairing together respective, by
    position, elements of the two input lists. If either list is exhausted 
    before the other (i.e., the lists do not contain the same number of 
    elements), then use the "error" function and state that the list has been
    exhausted. (You cannot use the Prelude's zip function in your solution.) -}
    
zip':: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' xs [] = error "2nd list is not exhaustive"
zip' [] ys = error "1st list is not exhaustive"
zip' (x:xs) (y:ys) = addLists [(x,y)] (zip' xs ys)

{- findPoint : takes in a point type and a list of points and returns a list
    all points that match the point type in the input list. -}
    
findPoint :: PointTy -> [Point a] -> [Point a]
findPoint ptype [] = []
findPoint ptype (x:xs)
    | getType x == ptype = addLists [x] (findPoint ptype xs)
    | otherwise = findPoint ptype xs

{- isValid : takes in a list of points and returns true if every point in the 
    list has the correct number coordinates. A Point2D has only two values, a 
    Point3D contains three values, and a Point4D only contains for 4 values. -}

isValid :: [Point a] -> Bool 
isValid [] = True
isValid (x:xs) = 
    if getType x == Point2D then
        if (length' $ getCoor x) == 2 then isValid xs
        else False
    else if getType x == Point3D then
        if (length' $ getCoor x) == 3 then isValid xs
        else False
    else if getType x == Point4D then
        if (length' $ getCoor x) == 4 then isValid xs
        else False
    else False
    
    
{- findRightTris : takes in a list of point lists and returns a Maybe list of
    the indices that represent the index of each point list that creates a 
    right triangle. A valid right triangle can only be composed of three 
    Point2D points. If a point list contains more than three points or has a
    mixture of other Point types other than Point2D then that point list is not
    valid (i.e., the function should return "Nothing"). Additionally, the 
    function needs to make sure the Point2D is valid (i.e., it should only 
    contain two values). You can use the Distance Formula and Pythagorean 
    Theorem to determine if the three points make a valid right triangle.-}
    
findRightTris :: [[Point Double]] -> Maybe [Integer]
findRightTris [] = Nothing
findRightTris a = if length' (findRightTrisHelper a 0) == 0
                      then Nothing
                  else Just (findRightTrisHelper a 0)




