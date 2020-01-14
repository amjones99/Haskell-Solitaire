module Solitaire1 where

{- COM2108 Solitaire assignment: 2018 assignment 2
   NAMING CONVENTIONS
   p a pip value
   s a suit
   w1,w2 random weights
   bd a board
   (fnds,cols,res) foundations, columns, reserve
   colHeads - cards at heads of columns
   
-}


 import System.Random
 import Data.List
 import Data.Maybe
 import Debug.Trace
 

 -- playing card data structures

 data Suit = Hearts|Clubs|Diamonds|Spades
             deriving (Eq, Show,Enum,Ord)
 
 allSuits = enumFrom Hearts -- list of enumerated items from arg
             
 data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
            deriving (Eq,Ord,Show,Enum)
 
 allPips = enumFrom Ace
            
                       
 type Card = (Pip,Suit)
 
 type Deck = [Card]
 
 ------------------------------------------------------------------  
-- 8 off solitaire data structures

 type EOBoard = (Foundations, Columns, Reserves)
                

 type Foundations = [Card] -- only need to know top card
 
 type Columns = [[Card]]
 
 type Reserves = [Card]
 
 --- alternative with newtype
 
 -- newtype EOBoardN = EOBoardN {fnds :: Foundations, cols :: Columns, res :: Reserves}
 
 newtype EOBoardN = EOB (Foundations,Columns, Reserves)

  
 -------------------------------------------------------------------
 
-- next card in suit
 
 sCard :: Card->Card
 sCard (p,s) = ((succ p),s)
 
-- predecessor in suit
 
 pCard :: Card->Card
 pCard (p,s) =((pred p),s)
 
 pack :: Deck
 
-- with comprehensions
 pack =  [(p,s)| p<- allPips,
                 s<- allSuits ]      
{-
-- with mapping fns
 pack = map (\ s->(map (\ p-> (s,p)) allPips)) allSuits
           
-- shuffle the pack
-}

 shuffle :: Int -> [Card]

 shuffle seed =  
  let
    gen= mkStdGen seed
    weights = take 52 (randoms gen :: [Int])
    dset = (map fst (sortBy  
               (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
               (zip pack weights)))
  in
   dset           

-----------------------------------------------------------------
  
-- 8 off deal, given seed

 eODeal :: Int-> EOBoard
 
 eODeal seed = 
  let
   spack = shuffle seed
   cols  = [(take 6 spack),          (take 6 (drop 6 spack)),  (take 6 (drop 12 spack)), (take 6 (drop 18 spack)), 
           (take 6 (drop 24 spack)), (take 6 (drop 30 spack)), (take 6 (drop 36 spack)), (take 6 (drop 42 spack))]
   fnds  = []
   res   = [spack!!48,spack!!49,spack!!50,spack!!51]
  in
   (fnds,cols,res)          


 

-----------------------------------------------------------------
-- isAce, isKing utilities

 isAce :: Card-> Bool
 isAce (p,_) = (p==Ace)
 
 isKing :: Card-> Bool
 isKing (p,_) = (p==King)
----------------------------------------------------------------
-- CARD MOVEMENT FNS
-- all take an EOBoard & return an EOBoard
--------------------------------------------------------------
-- toFoundations
-- move everthing that will go to foundations

 toFoundations :: EOBoard -> EOBoard
 
 toFoundations bd = 
  toFA (reserveAcesToFoundations bd) -- because reserveAcesToFoundations is called only once
  
 toFA :: EOBoard -> EOBoard -- called recursively till no change
 
 toFA bd
  | bd/=cafBd = toFA cafBd
  | bd/=rtfBd = toFA rtfBd
  | bd/=chfBd = toFA chfBd
  |otherwise = bd
  where
   cafBd=(colAcesToFoundations bd) --- could trace here .. traceShow bd (colAcesToFoundations bd
   rtfBd=reserveToFoundations bd
   chfBd=colHeadsToFoundations bd
   
   
------------------------------------------------------------
-- reserveAcesToFoundations
-- move all aces from reserve to foundations
-- can only happen at start of game, first thing that happens,foundations must be []

 reserveAcesToFoundations :: EOBoard -> EOBoard
 
 reserveAcesToFoundations (fnds,cols,res)
  |(null fnds) = ((filter isAce res),cols,(filter (not.isAce) res)) -- composition there
  |otherwise = (fnds,cols,res)
 
------------------------------------------------------------
--reserveToFoundations
-- non-Aces from reserve to foundations

 reserveToFoundations :: EOBoard -> EOBoard
 
 reserveToFoundations (fnds,cols,res) = 
      ((map (\f-> (if (not (isKing f))&&(elem (sCard f) res) then (sCard f) else f)) fnds), -- update fnds
       cols,
       (filter (\r-> (not(elem (pCard r) fnds )))res))  --update res
       
------------------------------------------------------------
-- column aces to foundations    

 colAcesToFoundations :: EOBoard -> EOBoard
 
 colAcesToFoundations (fnds,cols,res) = 
  (fnds ++ (filter isAce colHeads),
  (filter (not.null) (map (\col -> if (isAce (head col)) then (tail col) else col) cols)),
   res)
   where
    colHeads = map head cols
------------------------------------------------------------
-- column heads to foundations

 colHeadsToFoundations :: EOBoard -> EOBoard
 
 colHeadsToFoundations (fnds,cols,res) = 
  ((map (\f-> if (not(isKing f))&&(elem (sCard f) colHeads) then (sCard f) else f) fnds), -- **f can't be king update foundations
   (filter (not.null) -- update columns
           (map (\col@(hc:rc)-> if (elem (pCard hc) fnds) then rc else col)cols)), -- may have emptied a column
   res) 
  where
   colHeads = map head cols 
   
----------------------------------------------------------------
-- example

 b1@(f1,c1,r1) = eODeal 21
 b2@(f2,c2,r2) = toFoundations b1  
   