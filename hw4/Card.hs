{- 
  File      :  Card.hs 
  Copyright : (c) Jack Gang, 05/01/18 
  Contains data types and functions for card-related data types.
-}

module Card
(
   Card(..),
   Hand,
   Deck,
   handValue,
   draw,
   updateDraw,
   compareHands
) where

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King 
            deriving (Eq, Show)             -- need Eq for elem and Show for output
             
-- Hand and Deck are both just lists of Cards
type Hand = [Card]
type Deck = [Card]

cardValue :: Card -> Integer
cardValue Ace = 11      -- default Ace to 11 for now
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10

-- first calculate hand value if Ace is 11
rawHandValue :: Hand -> Integer
rawHandValue lst = foldl (\acc x -> cardValue x + acc) 0 lst

-- then convert Aces to 1's if necessary
handValue :: Hand -> Integer
handValue hand = let raw = rawHandValue hand
                 in
                 if (elem Ace hand) && raw > 21 then handValue (removeAce hand) + 1
                 else raw

-- helper function to remove Aces
removeAce :: Hand -> Hand
removeAce [] = []
removeAce (x:xs) = if x == Ace then xs
                   else [x] ++ removeAce xs

-- return first Card in a Deck
draw :: Deck -> Card
draw lst = head lst

-- removes the first Card from a Deck
updateDraw :: Deck -> Deck
updateDraw (x:xs) = xs

-- compare two hand values to see which won (or a push)
compareHands :: Hand -> Hand -> Int
compareHands h1 h2 = if handValue h1 > handValue h2 then 1
                     else if handValue h1 < handValue h2 then 2
                     else 0
                 