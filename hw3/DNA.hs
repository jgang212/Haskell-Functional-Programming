{- 
  File      :  DNA.hs 
  Copyright : (c) Jack Gang, 04/17/18 
  Contains functions for DNA data types.
-}

{- 
   --- Definitions for Base, BasePair, Strand and Helix 
-}

data Base = A | C | G | T 
data BasePair = BasePair Base Base
data Strand = Strand [Base]
data Helix = Helix [BasePair]

{- 
    Define Show and Eq instances for Base, BasePair, Strand, and Helix. 

    The show for a "Base" value should be the strings: "A" or "T" or "C" or "G"
    The show for a "BasePair" value should be formatted using tuple syntax with the base letter. For example: "(A,T)" or "(G,C)"
    The show for a "Strand" value should be formatted using list syntax with the base letters. For example: "[A,B,C,T]"
    The show for a "Helix" value should be formatted using tuple syntax and list syntax with the base letters. For example: "[(A,B),(C,T)]"

    Hint: Remember you can use the "++" to combine strings together 
-}

instance Show Base where    
    show A = "A" 
    show C = "C" 
    show G = "G" 
    show T = "T" 
    
instance Show BasePair where    
    show (BasePair b1 b2) = "(" ++ show b1 ++ "," ++ show b2 ++ ")"

instance Show Strand where
    show (Strand (x:xs)) = show (x:xs)

instance Show Helix where
    show (Helix (x:xs)) = show (x:xs)
    
instance Eq Base where
    A == A = True
    C == C = True
    G == G = True
    T == T = True
    _ == _  = False 
    
instance Eq BasePair where
    (==) (BasePair b1 b2) (BasePair b3 b4) =
            b1 == b3 && b2 == b4
    
instance Eq Strand where
    (==) (Strand bList1) (Strand bList2) =
            bList1 == bList2
            
instance Eq Helix where
    (==) (Helix bpList1) (Helix bpList2) =
            bpList1 == bpList2

{-
    wccHelix :: Strand -> Helix. 
    Given a Strand, generate a Helix.

    Requirements 
    -------------
    1. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    2. You can define helper functions but they can only use HOFs if they return lists.  
    3. Although we did not cover it in class, you are allowed to use list comprehension but it's not required. 

    Hint: This function can be written in one line.

    Example: 
    Main*> wccHelix (Strand [A,T,C,G]) 
    [(A,T),(T,A),(C,G),(G,C)] 
-} 

wccHelix :: Strand -> Helix
wccHelix (Strand bases) = Strand [map show bases]

{- 
    makeHelix :: String -> Helix. 
    Given a String of base letters, make a Helix.

    Requirements 
    -------------
    1. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    2. You can define helper functions but they can only use HOFs if they return lists.  
    3. You can reuse functions defined in this file. 
    4. Although we did not cover it in class, you are allowed to use list comprehension but it's not required. 

    Hint: This function can be written in one line.


    Example: 
    Main*> makeHelix “ACTG" 
    [(A,T),(C,G),(T,A),(G,C)]
-} 


{- 
   willAnneal :: Strand -> Strand -> Bool. 
   Determine whether two strands will perfectly anneal (i.e., every base is a Watson-Crick complementarity).
   
    Requirements 
    -------------
    1. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.  
    2. You can define helper functions but they can only use HOFs, if they return lists.  
    3. You can reuse functions defined in this file. 


   Main*> willAnneal (Strand [A,C,T,G]) (Strand [T,G,A,C]) 
   True
   Main*> willAnneal (Strand [A,C,T,T]) (Strand [T,G,A,C])
   False
-} 
