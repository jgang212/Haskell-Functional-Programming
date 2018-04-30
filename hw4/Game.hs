{- 
  File      :  Game.hs 
  Copyright : (c) Jack Gang, 05/01/18 
  Contains data types and init function for game-related data types.
-}

module Game
(
   Player(..),
   GameStatus(..),
   Game(..),
   initGame
) where

-- for Deck and Hand
import Card

-- cardCount only used for output and to check for BlackJacks
data Player = Player {cardCount :: Int, hand :: Hand}  

data GameStatus = PlayerTurn | DealerTurn | Starting
                  deriving (Eq)     -- need Eq to check GameStatus when going through game loop

data Game = Game {user :: Player,  dealer :: Player, status :: GameStatus, deck :: Deck}

-- start new game (deck must be a shuffled full deck)
initGame :: Deck -> Game
initGame deck = Game (Player 0 []) (Player 0 []) Starting deck 