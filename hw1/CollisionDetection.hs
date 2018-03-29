{- 
  File      :  CollisionDetection.hs 
  Copyright : (c) Jack Gang, 04/03/18 
  Contains types for representing 2D objects and collision functions for them.  
  Sources for collision algorithms: 
    http://devmag.org.za/2009/04/13/basic-collision-detection-in-2d-part-1/
    http://devmag.org.za/2009/04/17/basic-collision-detection-in-2d-part-2/
-}

{--------------------------------------------------------}

{- Collision Detection Types -}

{- type alias for representing a Point as a tuple of Doubles -}
type Point = (Double, Double)

{- type alias for representing a LineSegment as a tuple of Points -}
type LineSegment = (Point, Point)

{- type alias for representing an axis-aligned BoundingBox as a tuple of four 
    LineSegments in order of left, top, bottom, right -}
type BoundingBox = (LineSegment, LineSegment, LineSegment, LineSegment)

{- type alias for representing a Circle as a tuple of a Point (center) and a 
    Double (radius) -}
type Circle = (Point, Double)

{--------------------------------------------------------}

{- Helper functions -}

{- getLSExtreme : This function returns the min/max x/y axis bounds of a line
    segment, given the input parameters -}
getLSExtreme :: LineSegment -> Bool -> Bool -> Double
getLSExtreme ls isMax isXAxis = if isXAxis then 
                                    if isMax then
                                        max (fst $ fst ls) (fst $ snd ls)
                                    else
                                        min (fst $ fst ls) (fst $ snd ls)
                                else
                                    if isMax then
                                        max (snd $ fst ls) (snd $ snd ls)
                                    else
                                        min (snd $ fst ls) (snd $ snd ls)
                                                                        
{- getDistance : This function returns the distance between two points -}
getDistance :: Point -> Point -> Double
getDistance pt1 pt2 = sqrt ((fst pt1 - fst pt2) ^ 2 + (snd pt1 - snd pt2) ^ 2)

{--------------------------------------------------------}
                                
{- Collision Detection Functions -}

{- rectsIntersect : This function returns true if two BoundingBoxes collide. -}
rectsIntersect :: BoundingBox -> BoundingBox -> Bool
rectsIntersect bb1 bb2 = let (lft1, top1, btm1, rgt1) = bb1
                             (lft2, top2, btm2, rgt2) = bb2
                         {- typical collision check -}
                         in (not ((snd $ fst btm1) > (snd $ fst top2) || 
                                 (snd $ fst top1) < (snd $ fst btm2) || 
                                 (fst $ fst lft1) > (fst $ fst rgt2) || 
                                 (fst $ fst rgt1) < (fst $ fst lft2))) &&
                         {- make sure one is not enclosed by another -}
                            (not ((snd $ fst top1) < (snd $ fst top2) && 
                                 (snd $ fst btm1) > (snd $ fst btm2) && 
                                 (fst $ fst lft1) > (fst $ fst lft2) && 
                                 (fst $ fst rgt1) < (fst $ fst rgt2))) &&
                            (not ((snd $ fst top1) > (snd $ fst top2) && 
                                 (snd $ fst btm1) < (snd $ fst btm2) && 
                                 (fst $ fst lft1) < (fst $ fst lft2) && 
                                 (fst $ fst rgt1) > (fst $ fst rgt2)))

{- rectLineIntersect : This function returns true if a LineSegment intersects 
    with a BoundingBox. -}
rectLineIntersect :: LineSegment -> BoundingBox -> Bool
rectLineIntersect ls bb = let (lft, top, btm, rgt) = bb
                          {- typical collision check -}
                          in (not ((snd $ fst btm) > getLSExtreme ls True False || 
                                 (snd $ fst top) < getLSExtreme ls False False || 
                                 (fst $ fst lft) > getLSExtreme ls True True || 
                                 (fst $ fst rgt) < getLSExtreme ls False True)) &&
                          {- make sure line is not enclosed by box -}
                             (not ((snd $ fst top) > getLSExtreme ls True False && 
                                 (snd $ fst btm) < getLSExtreme ls False False && 
                                 (fst $ fst lft) < getLSExtreme ls False True && 
                                 (fst $ fst rgt) > getLSExtreme ls True True))
         
{- circlesIntersect : This functions returns true if two Circles collide. -}
circlesIntersect :: Circle -> Circle -> Bool
                         {- typical collision check (including tangent) -}
circlesIntersect c1 c2 = (getDistance (fst c1) (fst c2) <= (snd c1 + snd c2)) &&
                         {- make sure one is not enclosed by another -}
                         (getDistance (fst c1) (fst c2) >= max (snd c1) (snd c2) -
                                                           min (snd c1) (snd c2))

{- circleLineIntersect : This function returns true if a LineSegment intersects
    with a Circle. -}
circleLineIntersect :: LineSegment -> Circle -> Bool
circleLineIntersect ls cir = let localP1 = ((fst $ fst ls) - (fst $ fst cir),
                                           (snd $ fst ls) - (snd $ fst cir))
                                 localP2 = ((fst $ snd ls) - (fst $ fst cir),
                                           (snd $ snd ls) - (snd $ fst cir))
                                 p2MinusP1 = (fst localP2 - fst localP1,
                                             snd localP2 - snd localP1)
                                 a = (fst p2MinusP1) ^ 2 + (snd p2MinusP1) ^ 2
                                 b = 2 * (fst p2MinusP1 * fst localP1 + 
                                         snd p2MinusP1 * snd localP1)
                                 c = fst localP1 ^ 2 + snd localP1 ^ 2 - snd cir ^ 2
                                 delta = b ^ 2 - 4 * a * c
                           in if delta >= 0 then True else False
                                                           
{--------------------------------------------------------}

{- TESTING -}

{- boxes -}
bbox1P1 = (1.0, 1.0)
bbox1P2 = (1.0, 4.0)
bbox1P3 = (5.0, 4.0)
bbox1P4 = (5.0, 1.0)
bbox1 = ((bbox1P1, bbox1P2), (bbox1P2, bbox1P3), (bbox1P1, bbox1P4), 
         (bbox1P3, bbox1P4))

bbox2P1 = (5.0, 4.0)
bbox2P2 = (5.0, 10.0)
bbox2P3 = (9.0, 10.0)
bbox2P4 = (9.0, 4.0)
bbox2 = ((bbox2P1, bbox2P2), (bbox2P2, bbox2P3), (bbox2P1, bbox2P4), 
         (bbox2P3, bbox2P4))

bbox3P1 = (2.0, 2.0)
bbox3P2 = (2.0, 3.0)
bbox3P3 = (4.0, 3.0)
bbox3P4 = (4.0, 2.0)
bbox3 = ((bbox3P1, bbox3P2), (bbox3P2, bbox3P3), (bbox3P1, bbox3P4), 
         (bbox3P3, bbox3P4))
         
bbox4P1 = (0.0, 0.0)
bbox4P2 = (0.0, 5.0)
bbox4P3 = (6.0, 5.0)
bbox4P4 = (6.0, 0.0)
bbox4 = ((bbox4P1, bbox4P2), (bbox4P2, bbox4P3), (bbox4P1, bbox4P4), 
         (bbox4P3, bbox4P4))
         
bbox5P1 = (1.0, 1.0)
bbox5P2 = (1.0, 4.0)
bbox5P3 = (6.0, 4.0)
bbox5P4 = (6.0, 1.0)
bbox5 = ((bbox5P1, bbox5P2), (bbox5P2, bbox5P3), (bbox5P1, bbox5P4), 
         (bbox5P3, bbox5P4))
         
bbox6P1 = (0.0, 0.0)
bbox6P2 = (0.0, 3.0)
bbox6P3 = (6.0, 3.0)
bbox6P4 = (6.0, 0.0)
bbox6 = ((bbox6P1, bbox6P2), (bbox6P2, bbox6P3), (bbox6P1, bbox6P4), 
         (bbox6P3, bbox6P4)) 
         
{- line segments -}
ls1 = ((1.0, 1.0), (4.0, 3.0))
ls2 = ((1.0, 1.0), (1.0, 4.0))

{- circles -}
cir1 = ((0.0, 0.0), 4.0)
cir2 = ((6.0, 6.0), 1.0)
cir3 = ((5.0, 5.0), 4.0)
cir4 = ((8.0, 0.0), 4.0)
cir5 = ((2.99, 0.0), 1.0)

{- Holds a list of test cases for the collision detection functions -}
testResults :: [String]
                  -- should say True due to collided point
testResults = let test1 = rectsIntersect bbox1 bbox2
                  -- should say False due to enclosed
                  test2 = rectsIntersect bbox1 bbox3
                  -- should say False due to enclosed
                  test3 = rectsIntersect bbox1 bbox4
                  -- should say True due to tangent
                  test4 = rectsIntersect bbox1 bbox5
                  -- should say True due to regular collision
                  test5 = rectsIntersect bbox1 bbox6
                  -- should say False due to regular non-collision
                  test6 = rectsIntersect bbox2 bbox3
                  -- should say True due to collided point
                  test7 = rectLineIntersect ls1 bbox1
                  -- should say True due to tangent
                  test8 = rectLineIntersect ls2 bbox1
                  -- should say True due to regular collision
                  test9 = rectLineIntersect ls2 bbox6
                  -- should say False due to regular non-collision
                  test10 = rectLineIntersect ls2 bbox2
                  -- should say False due to enclosed
                  test11 = rectLineIntersect ls2 bbox4
                  -- should say False due to regular non-collision
                  test12 = circlesIntersect cir1 cir2
                  -- should say True due to regular collision
                  test13 = circlesIntersect cir1 cir3
                  -- should say True due to tangent
                  test14 = circlesIntersect cir1 cir4
                  -- should say False due to enclosed
                  test15 = circlesIntersect cir1 cir5
                  -- should say True due to regular collision
                  test16 = circleLineIntersect ls1 cir1
                  -- should say False due to regular non-collision
                  test17 = circleLineIntersect ls1 cir5
              in [show test1, show test2, show test3, show test4, show test5,
                  show test6, show test7, show test8, show test9, show test10,
                  show test11, show test12, show test13, show test14, 
                  show test15, show test16, show test17]
              
              