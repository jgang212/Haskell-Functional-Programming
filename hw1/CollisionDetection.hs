{- 
  File      :  CollisionDetection.hs 
  Copyright : (c) Jack Gang, 04/03/18 
  Contains types for representing 2D objects and collision functions for them.  
  Sources for collision algorithms: 
    http://devmag.org.za/2009/04/13/basic-collision-detection-in-2d-part-1/
-}


{- Collision Detection Types -}

{- type alias for representing a Point as a tuple of Doubles -}
type Point = (Double, Double)

{- type alias for representing a LineSegment as a tuple of Points -}
type LineSegment = (Point, Point)

{- type alias for representing an axis-alined BoundingBox as a tuple of four 
    LineSegments in order of left, top, bottom, right -}
type BoundingBox = (LineSegment, LineSegment, LineSegment, LineSegment)

{- type alias for representing a Circle as a tuple of a Point (center) and a 
    Double (radius) -}
type Circle = (Point, Double)


{- Collision Detection Functions -}

{- rectsIntersect : This function returns true if two BoundingBoxes collide. -}
rectsIntersect :: BoundingBox -> BoundingBox -> Bool
rectsIntersect bb1 bb2 = let (lft1, top1, btm1, rgt1) = bb1
                             (lft2, top2, btm2, rgt2) = bb2
                         in not ((snd $ fst btm1) > (snd $ fst top2) || 
                                 (snd $ fst top1) < (snd $ fst btm2) || 
                                 (fst $ fst lft1) > (fst $ fst rgt2) || 
                                 (fst $ fst rgt1) < (fst $ fst lft2))                                 
                                 
{- Holds a list of test cases for the collision detection functions -}
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

testResults :: String
                  -- should say True due to collided point
testResults = let test1 = rectsIntersect bbox1 bbox2
              in show test1           