module Solitaire2 where 
 import Solitaire1
 import MeanStats 
 import Data.List 
 import MergeSort
 import Data.Maybe
 import System.Random
 
 -- eOExpt - Plays 100 games and returns the total wins and the average score
 -- The dealt boards are randomised by an integer input
 eOExpt :: Int -> (Int,Float)
 eOExpt i = (occur,avScore)
  where lScores = map eOGame (map eODeal (take 100 (randoms (mkStdGen i) :: [Int])))
        avScore = fromIntegral (sum lScores)/ 100
        occur = length (filter (== 52) lScores)

 -- eOGame - plays a game and returns the score at the end
 eOGame :: EOBoard -> Int
 eOGame b@(f,c,r)
  |length r == 0 && length c == 0 = score b
  |isNothing nBoard = score b
  |otherwise = eOGame (fromJust nBoard)
  where nBoard = chooseMove b

 -- score - returns number of cards in the foundations of a board
 score :: EOBoard -> Int
 score (f,c,r) = 52 - (length r) - (foldr (+) 0 (map length c))

 -- chooseMove - Uses lookAhead to get all available moves and chooses the best move based on calculated weights
 -- by mapping with the addWeight function
 chooseMove :: EOBoard -> Maybe EOBoard
 chooseMove b
  |allMoves == []  = Nothing
  |otherwise = Just (head (map fst (mergesort (\(_,n1) (_,n2)-> n1 >n2) (zip allMoves (map addWeight allMoves)))))
  where allMoves = map toFoundations (lookAhead b)

 -- addWeight - Calculated the weight of a board by finding its score minus a penalty of the number of reserves
 -- and adding that result adding to the number of columns which have a king as their last (bottom) card
 addWeight :: EOBoard -> Int 
 addWeight b@(f,c,r) = score b - length r + length (filter (\col -> isKing(last col)) c)
 
 --lookAhead - Returns a list of boards/moves that either look one move ahead or just this current move
 --It uses findmoves to find the current moves (mainMove) and uses it again when finding the moves for lookMove
 lookAhead :: EOBoard -> [EOBoard]
 lookAhead b
  |lookMove /= [] = lookMove
  |mainMove /= [] = mainMove
  |otherwise = []
  where mainMove = map toFoundations (findMoves b) -- mapping to foundations 
        lookMove = map toFoundations (concat (map findMoves (mainMove)))

 -- findMoves - Returns the outputs of rMoves, cMoves and stackReserve (when mapped against the moveable cards) that are different to the initial board
 findMoves :: EOBoard -> [EOBoard]
 findMoves b@(f,c,r) = filter (/= b) ((map (rMoves b) allMoveableCards) ++ (map (cMoves b) allMoveableCards) ++ (map (stackReserve b) allMoveableCards))
  where colHeads = [head l | l <- c, l /= []]
        allMoveableCards = r ++ colHeads

 -- rMoves - Moves a card to the reserves if the card it is checking is not in the reserves already, rMovesB returns flase and is not part of a successive stack 
 -- starting with a king at the bottom (inc king-only column)
 rMoves:: EOBoard -> Card -> EOBoard
 rMoves b@(f,c,r) card 
  |(length r > 8) || (elem card r) || (rMovesB b card) || (onlyKing b card) || (kingStack b card) = b 
  |otherwise = (f, filter (not.null) (map (filter (/= card)) c), r++[card]) 

 -- rMovesB - Returns true if the passed card is part of a stack (i.e. card behind is not its successor)
 rMovesB :: EOBoard -> Card -> Bool
 rMovesB b@(_,[],_) card = False  
 rMovesB b@(f,(h:t),r) card  
  |(length h > 1) && (not(isKing card)) && (head h == card) && (head (tail h) == sCard card) = True
  |otherwise = rMovesB (f,t,r) card
  
 -- onlyKing - If the passed card is the only card in its column this returns true
 onlyKing :: EOBoard -> Card -> Bool 
 onlyKing b@(_,[],_) card = False 
 onlyKing b@(f,c,r) card = singleKing
  where singleCol = map head (filter (\col -> (length col) == 1) c)
        singleKing = (isKing card) && (singleCol /= []) && (elem card singleCol)

 -- kingStack - If the bottom card of the passed card's column is a king and all the above cards are predecessors in order return true
 kingStack :: EOBoard -> Card -> Bool 
 kingStack b@(_,[],_) _ = False  
 kingStack b@(f,(h:t),r) card  
  |(length h == length stack) && (last h == last stack) && (isKing (last h)) && (isKing (last stack)) = True 
  |otherwise = kingStack (f,t,r) card 
  where stack = colStacks b card
  
 -- colStacks - Gets a list of all cards within a stack in a column (if any) 
 colStacks :: EOBoard -> Card -> [Card] 
 colStacks b@(_,[],_) card = []
 colStacks b@(f,(h:t),r) card 
  |(head h == card) && (isKing card) = [card] 
  |(head h == card) && (length h == 1) = [card] 
  |head h == card = [card] ++ colStacks (f,[tail h],r) (sCard card) 
  |otherwise = colStacks (f,t,r) card

 -- cMoves - Moves a card to a column (from either reserves or other columns) if move is available
 -- Moves the card to an empty column if the card is a King, not already in a singleKing column and there is a free column to move it to
 -- Moves card to the top of another column if the current head of that column is its successor
 cMoves:: EOBoard -> Card -> EOBoard
 cMoves b@(f,c,r) card
  |(isKing card) && (length c < 8) && not(onlyKing b card) = (f,filter (not . null) ([card]:delFromCol), delete card r) 
  |(isKing card /= True) && (elem (sCard card) colHeads) = (f,[if (head c) == (sCard card) then card:c else c| c <- delFromCol, c /= []],delete card r)
  |otherwise = b
  where delFromCol =  map (filter (/= card)) c
        colHeads = [head l | l <- c, l /= []] 

 -- stackReserve - Moves stack of successive cards in a column to the reserves if there is enough space in reserves  
 stackReserve :: EOBoard -> Card -> EOBoard
 stackReserve b@(f,c,r) card 
  |kingStack b card = b
  |(8 - length r > length stack) && (rMovesB b card) = (f, filter (not . null) deleteCard, r ++ stack)
  |otherwise = b
  where deleteCard =  [if elem card col then drop (length stack) col else col | col <- c]
        stack = colStacks b card