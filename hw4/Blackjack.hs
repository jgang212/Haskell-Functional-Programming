{- 
  File      :  Blackjack.hs 
  Copyright : (c) Jack Gang, 05/01/18 
  Contains the game IO functions for Blackjack.
-}

import Game
import Shuffle
import Card

-- user choice to hit or stay
hitOrStay :: IO Int
hitOrStay = do 
    putStrLn "What would you like to do?"
    putStrLn "(1) Hit"
    putStrLn "(2) Stay"
    optStr <- getLine 
    let optionInt = read optStr :: Int 
    return optionInt 

-- user choice to play again or not, exits if no and restarts if yes
gameOver :: IO ()
gameOver = do
    putStrLn "Would you like to play again?"
    putStrLn "(1) Yes"
    putStrLn "(2) No"
    optStr <- getLine 
    let optionInt = read optStr :: Int 
    if optionInt == 2 then do
        return ()
    else do
        startNewGame

-- start a new game with a full shuffled deck
startNewGame :: IO ()
startNewGame = do
    deck <- shuffle [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King,
                     Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King,
                     Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King,
                     Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    let newGame = initGame deck
    gameLoop newGame

main :: IO () 
main = do 
    putStrLn "Welcome to the game of BlackJack!"
    startNewGame

-- main game loop: separated into parts depending on the gameState
gameLoop :: Game -> IO () 
                     -- Starting: special actions for the beginning of the game
gameLoop gameState = if status gameState == Starting then do
                        let updatedDeck = deck gameState
                        putStrLn "------------------------------------------------"
                        putStrLn "Drawing initial hands for both user and dealer."

                        -- draw 2 cards for user and 2 cards for dealer
                        let userCard1 = draw updatedDeck
                        putStrLn ("User card 1: " ++ (show userCard1))  
                        let updatedDeck1 = updateDraw updatedDeck                        
                        let newGameState1 = Game (Player 1 [userCard1]) (Player 0 []) Starting updatedDeck1

                        let userCard2 = draw updatedDeck1
                        putStrLn ("User card 2: " ++ (show userCard2))
                        let updatedDeck2 = updateDraw updatedDeck1
                        let newGameState2 = Game (Player 2 [userCard1, userCard2]) (Player 0 []) Starting updatedDeck2

                        let dealerCard1 = draw updatedDeck2
                        putStrLn ("Dealer card 1: " ++ (show dealerCard1))
                        let updatedDeck3 = updateDraw updatedDeck2
                        let newGameState3 = Game (Player 2 [userCard1, userCard2]) (Player 1 [dealerCard1]) Starting updatedDeck3

                        let dealerCard2 = draw updatedDeck3
                        putStrLn ("Dealer card 2: X")   -- don't show 2nd dealer card
                        let updatedDeck4 = updateDraw updatedDeck3
                        let newGameState4 = Game (Player 2 [userCard1, userCard2]) (Player 2 [dealerCard1, dealerCard2]) PlayerTurn updatedDeck4

                        -- check for immediate loss if dealer has Blackjack
                        if handValue (hand (dealer newGameState4)) == 21 then do
                            putStrLn "------------------------------------------------"
                            putStrLn ("Dealer has BlackJack. You lose.")
                            putStrLn ("Dealer cards: " ++ (show (hand (dealer newGameState4))))
                            gameOver
                        else do
                            gameLoop newGameState4      -- loop again, but with gameState = PlayerTurn

                     -- PlayerTurn: hit or stay
                     else if status gameState == PlayerTurn then do
                        putStrLn "------------------------------------------------"
                        -- check for player Blackjack (works because we checked already for dealer Blackjack earlier)
                        if cardCount (user gameState) == 2 && handValue (hand (user gameState)) == 21 then do
                            putStrLn ("Congratulations! You win with Blackjack!")
                            putStrLn ("User cards: " ++ (show (hand (user gameState))))
                            gameOver
                        -- no Blackjack, must decide
                        else do
                            decision <- hitOrStay
                            let gameDeck = deck gameState  
                            -- Hit                      
                            if decision == 1 then do
                                let hitCard = draw gameDeck
                                let count = cardCount (user gameState) + 1
                                let newHand = hand (user gameState) ++ [hitCard]
                                putStrLn ("You got a: " ++ (show hitCard))
                                putStrLn ("User cards: " ++ (show newHand))
                                -- check for bust after hit
                                if handValue newHand > 21 then do
                                    putStrLn ("Sorry, you busted with " ++ show (handValue newHand) ++ ". You lose.")
                                    gameOver
                                -- game loop for PlayerTurn again (can decide hit/stay again)
                                else do
                                    let updatedDeck = updateDraw gameDeck                        
                                    let newGameState = Game (Player count newHand) (dealer gameState) PlayerTurn updatedDeck
                                    gameLoop newGameState
                            -- Stay (go to DealerTurn)
                            else do
                                let newGameState = Game (user gameState) (dealer gameState) DealerTurn gameDeck
                                gameLoop newGameState
                     -- DealerTurn: dealer follows set rules for hitting or staying
                     else if status gameState == DealerTurn then do
                        putStrLn "------------------------------------------------"
                        putStrLn ("Dealer cards: " ++ (show (hand (dealer gameState))))
                        let dealerHandValue = handValue (hand (dealer gameState))
                        -- dealer stays > 18 or on soft 17
                        if (dealerHandValue >= 18) || (dealerHandValue == 17 && ((elem Ace (hand (dealer gameState))) == False)) then do
                            putStrLn ("Dealer stays at " ++ show dealerHandValue ++ ".")
                            let result = compareHands (hand (user gameState)) (hand (dealer gameState))
                            if result == 1 then do
                                putStrLn ("Congratulations! You win!")
                            else if result == 2 then do
                                putStrLn ("Sorry, you lose.")
                            else do
                                putStrLn ("Push.")
                            putStrLn ("User cards: " ++ (show (hand (user gameState))) ++ ", User value: " ++ (show (handValue (hand (user gameState)))))
                            putStrLn ("Dealer cards: " ++ (show (hand (dealer gameState))) ++  ", Dealer value: " ++ show dealerHandValue)
                            gameOver
                        -- dealer must hit
                        else do
                            let gameDeck = deck gameState
                            let hitCard = draw gameDeck
                            let count = cardCount (dealer gameState) + 1
                            let newHand = hand (dealer gameState) ++ [hitCard]
                            putStrLn ("Dealer cards: " ++ (show newHand))
                            -- check for bust after hit
                            if handValue newHand > 21 then do
                                putStrLn ("Congratulations! Dealer busted with " ++ show (handValue newHand) ++ "! You Win!")
                                gameOver
                            -- game loop for DealerTurn again (can decide hit/stay again)
                            else do
                                let updatedDeck = updateDraw gameDeck                        
                                let newGameState = Game (user gameState) (Player count newHand) DealerTurn updatedDeck
                                gameLoop newGameState
                     -- should never happen, for exhaustiveness
                     else return ()