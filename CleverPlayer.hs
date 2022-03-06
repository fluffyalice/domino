{-# LANGUAGE TupleSections #-}
{--
 author: Ruiqing Xu
 time: 12.13
 email: rxu22@sheffield.ac.uk
--}

module CleverPlayer (cleverPlayer) where 
import DomsMatch
import Data.Maybe(fromJust) 
import Data.List ( maximumBy,(\\),minimumBy,union ,sortBy,intersect)

--------------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------------

-- Define type aliases to make function signatures easy to read
type Me = Player -- me
type MyHand = Hand -- my cards in hand
type Opponent = Player -- opponent
type OppoHand = Hand -- cards in opponent hand

{--Tatic allows return [], indicating that the play cannot be found, and DomsPlayer must give a play.
This allows DomsPlayer to choose between multiple Tatic options based on the surveyed returns.--}
type Tactic = OppoHand->MyHand->DominoBoard->Me->Scores->[(Domino,End)]

--------------------------------------------------------------------------------------
-- Comprehensive strategy
--------------------------------------------------------------------------------------

-- CleverPlayer combines all of the tactics , and this is the main player which gonna play the game
-- See design.txt for its logic
cleverPlayer::DomsPlayer
cleverPlayer myHand board me scores@(s1,s2)
    |board == InitBoard && scores ==(0,0) = (firstPlay me myHand,L) -- Because it's the first step on the board. play on L or R, whatever you want
    |not (null bombs) = head bombs 
    |badHand board myHand = badHandPlayer myHand board me scores
    |null blkOpponent && null vSmartGreedy = randomPlayer myHand board me scores -- In this case, I will definitely lose because all the cards help my opponent score 61 points
    |null blkOpponent = head vSmartGreedy 
    |null vSmartGreedy = head randTactic-- block Opponent
    -- both blkOpponent and vSmartGreedy are not empty: 
    |otherwise = if myScore < oppoScore then 
                    case blkOpponent of   -- block Opponent strategy takes precedence
                        [x] -> x 
                        _ -> head $ sortByMyNextScore myScore board me $ blkOpponent 
                 else head vSmartGreedy -- Greedy strategy
    where bombs = arrive61 board myHand myScore -- win if I have the bomb
          myScore = if me==P1 then s1 else s2 
          oppoScore = if me==P1 then s2 else s1 
          blkOpponent = blockOpponent oppoHand myHand board me scores
          oppoHand = guessOppoHand scores board me myHand 
          vSmartGreedy = verySmartGreedy oppoHand myHand board me scores 
          randTactic = randomTactic oppoHand myHand board me scores 

--------------------------------------------------------------------------------------
-- Opening strategy
--------------------------------------------------------------------------------------
-- The starting score is (0,0), and which card is the best when it's my turn to play the first card?
-- Answer: Pick out the card from my hand that has scored the biggest difference between my opponent's next score
firstPlay::Player->Hand->Domino -- [(Domino,Int)]
firstPlay me myHand = fst $ maximumBy (\x1 x2->snd x1 `compare` snd x2) xs
    where xs = zip myHand $ map (scoreDiff oppoHand me) myHand
          oppoHand = domSet \\ myHand

--------------------------------------------------------------------------------------
-- Greedy strategy
--------------------------------------------------------------------------------------

-- The idiot greedy strategy: Always plays the highest card, regardless of whether the opponent gets a higher score on my hand
idiotGreedyPlayer::DomsPlayer
idiotGreedyPlayer myHand board me _ = snd $ maximumBy f (ls++rs)  -- All the cards I can play range from high score to low score and then pick the highest card
    where (poss1,poss2) = possPlays myHand board -- All the cards I can play
          ls = map (playDomScore L) poss1 -- Find the cards which I can play to the left side and count them one by one
          rs = map (playDomScore R) poss2 -- Find the cards which I can play to the right side and count them one by one
          f x y = fst x `compare` fst y -- The sorting function
          playDomScore end domino = (score,(domino,end)) 
              where score = scoreBoard $ fromJust $ playDom me domino board end

--smart and greedy strategy: Always play the cards that have the biggest margin of victory over my opponent 
--(i.e., consider whether my opponent can take advantage of my cards to score more points)
smartGreedyPlayer :: DomsPlayer 
smartGreedyPlayer myHand board me scores 
    = fst $ maximumBy g myPossResults
    where (poss1,poss2) = possPlays myHand board --All the cards I can play
          ls = map (,L) poss1
          rs = map (,R) poss2 
          myPossPlays = ls ++ rs 
          oppoHand = guessOppoHand scores board me myHand
          myPossResults = zip myPossPlays $ map (scoreDiff2 oppoHand board me) myPossPlays --count the consequences of all the cards I can play  
                                                                                           --(in order to consider if the opponent earn more points)
          g x y = snd x `compare` snd y --The sorting function

--Very smart and greedy strategy: it has the same ability of the smart and greedy strategy, 
--The difference is that it can also try to filter out the cards that help the opponent score 61 points after I play cards
--Notice: This is a real fight strategy, the other two greedy strategies are not strong enough
verySmartGreedy::Tactic 
verySmartGreedy oppoHand myHand board me scores
    = map fst $ reverse $ sortBy g $ myPossResults
    where 
          badDoms = concatMap (helpOppoWin oppoHand scores board me) myHand --The cards that help the opponent score 61 points after I played cards
          (poss1,poss2) = possPlays myHand board --All the cards I can play
          ls = map (,L) poss1
          rs = map (,R) poss2
          myPossPlays = (ls ++ rs) \\ badDoms --Maybe empty, that is, the cards in my hand can help the opponent reach 61 points
          myPossResults = zip myPossPlays $ map (scoreDiff2 oppoHand board me) myPossPlays --count the consequences of all the cards I can play  
                                                                                           --(in order to consider if the opponent can earn more points)
          g x y = snd x `compare` snd y --The sorting function

--------------------------------------------------------------------------------------
-- Bad Hand Player
--------------------------------------------------------------------------------------

-- Bad Hand Player
-- A bad card is one on which no score can be scored (each card is played for more than 61 points)
-- The main implement is using Block Opponent Strategy. Enter the next round in order to achieve the purpose of changing cards.
badHandPlayer :: DomsPlayer
badHandPlayer myHand board me scores 
    | not $ null dblBlock = head dblBlock 
    | not $ null blkOpponent = head blkOpponent 
    | not $ null zg = head zg 
    | otherwise = head $ opponentLowGain oppoHand myHand board me scores --play a card to give the opponent low score
    where oppoHand = guessOppoHand scores board me myHand
          dblBlock =  doubleBlock oppoHand myHand board me scores 
          blkOpponent = blockOpponent oppoHand myHand board me scores 
          zg = opponentZeroGain oppoHand myHand board me scores 

--------------------------------------------------------------------------------------
-- Blocking strategy
--------------------------------------------------------------------------------------     

--A strategy of returning a low score to an opponent
--The data returned is a sorted list, and the strategy at the head of the list gives the opponent the lowest score
opponentLowGain ::Tactic 
opponentLowGain oppoHand myHand board me scores 
    = map snd $ sortBy f $ concatMap (oppoLowGain oppoHand scores board me) myHand 
    where f x y = fst x `compare` fst y  

--A strategy that prevents an opponent from scoring
opponentZeroGain ::Tactic 
opponentZeroGain oppoHand myHand board me scores 
    = concatMap (oppoZeroGain oppoHand scores board me) myHand

--A strategy to block both players     
doubleBlock::Tactic 
doubleBlock oppoHand myHand board me scores 
    = dominos1 `intersect` dominos2
    where dominos1 = blockMyself [] myHand board me scores 
          dominos2 = blockOpponent oppoHand myHand board me scores 

--A strategy that block myself
blockMyself::Tactic 
blockMyself _ myHand board me _ 
    = concatMap blockMyself myHand 
    where blockMyself domino = tryBlock me domino board (myHand\\[domino])

--A strategy that blocks the opponent
blockOpponent::Tactic
blockOpponent oppoHand myHand board me _
    = concatMap blockOpponent myHand
        where blockOpponent domino = tryBlock me domino board oppoHand  -- a domino can block opponent


--------------------------------------------------------------------------------------
--Guess card algorithm
--------------------------------------------------------------------------------------

--Guess the opponent's cards
guessOppoHand::Scores->DominoBoard->Me->MyHand->OppoHand 
guessOppoHand _ InitBoard _ myHand = domSet \\ myHand 
guessOppoHand scores board@(Board _ _ history) me myHand = oppoHand2
    where 
        opponent = if me == P1 then P2 else P1  --my opponent
        hwe = addEnd history --Do some processing of the played history, including sorting and adding L and R
        dominosOnBoard = map fst3 history --A card that has been played on the board
        oppoHand1 = (domSet \\ myHand) \\ dominosOnBoard -- OppoHand1 contains ten undistributed sleep cards
        oppoHand2 = historyRecur isZero InitBoard hwe opponent oppoHand1 -- HistoryRecur tries to clear some of the sleeping cards from oppoHand1
        isZero = scores == sumScore board --True means the starting score is (0,0).
        
--Repeat history, brushing out cards the opponent may not have 
--(if Opponent have the card, they will play them in history for a higher score)
historyRecur :: Bool -> DominoBoard->HistoryWithEnd -> Opponent -> OppoHand -> OppoHand
historyRecur _ _ [] _ hand = hand --if there is no history repeat then return hand

--If the board is empty (InitBoard)
--If the opponent doesn't play the (5,4) card that the code suggests, can I guess he doesn't have this card? What about other case?
--Answer: according to the function firstPlay2, (5,4) is the only suggested card, and any other card that is played is likely to give the opponent a higher score than me.
historyRecur isZero InitBoard (h:hs) oppo oppoHand 
    = historyRecur isZero board' hs oppo oppoHand2 
    where board' = fromJust $ playDom (snd3 h) (fst3 h) InitBoard (trd3 h) 
          oppoHand2 = if isZero && snd3 h == oppo && fst3 h /=(5,4) then --If the initial score is (0,0) && the first card is played by the opponent && the opponent does not play (5,4)
                            oppoHand \\ [(5,4)]  
                      else oppoHand 

historyRecur isZero board@(Board _ _ hty) (h:hs) oppo oppoHand 
    |snd3 prePlay == me && snd3 h == me -- Because If I play two cards in a row, that means the opponent is blocked, and I guess the opponent doesn't have the suitable cards
            = historyRecur isZero board' hs oppo oppoHand1
    |snd3 prePlay == me && snd3 h == oppo -- I play and then my opponent plays, and I can guess from the score that my opponent has no cards with a higher score 
                                          -- (inaccurate, not to assume that the opponent is always greedy, but tests have shown a significant improvement in the win rate)
            = historyRecur isZero board' hs oppo oppoHand2
    |otherwise -- Other conditions are not conducive to guessing that the opponent does not have a suitable card
            = historyRecur isZero board' hs oppo oppoHand
    where me = if oppo == P1 then P2 else P1 
          prePlay = maximumBy cp hty --The last card played on the board
          cp x y = trd3 x `compare` trd3 y --Compare MoveNum
          pp = un $ possPlays oppoHand board --pp is the card an opponent can play
          un (xs,ys) = xs `union` ys --Combine all the cards the opponent can play, regardless of whether they play to the left side or right side
          board' = fromJust $ playDom (snd3 h) (fst3 h) board (trd3 h) --play card H on the board
          oppoHand1 = oppoHand \\ pp --Delete all cards that can be played
          oppoHand2 = oppoHand \\ betters -- Remove all cards with higher scores: assume the opponent has been using a idiot's greedy strategy (bug - this assumption is not correct, maybe it should be deleted)
          betters = filter better oppoHand -- All cards that can be played with a higher score
          better domino = playDomMaxScore oppo board domino > scoreBoard board' --play s domino card score is higher than play an H card score

--Add End(L or R) to the elements in the history and sort MoveNum from small to large
--The purpose is to facilitate the use of historyRecur
type HistoryWithEnd = [(Domino,Player,End)] 
addEnd::History->HistoryWithEnd
addEnd his = addEnd' his []
    where 
        addEnd'::History->HistoryWithEnd->HistoryWithEnd
        addEnd' [] hwe = hwe
        addEnd' his@(h:_) hwe = addEnd' his' hwe'
            where
                lastPlay@(a,b,_) = maximumBy cp his 
                cp x y = trd3 x `compare` trd3 y  
                his' = filter (/=lastPlay) his 
                hwe' |lastPlay == h = (a,b,L) : hwe
                     |otherwise = (a,b,R) : hwe 

--This function does not participate in the play, just analysis before the play of cards
--After the analysis of this function, (5,4) is the only card that my opponent cannot play with a higher score after I play on the board 
--(suppose I play a card and the opponent can play any card other than this card).
firstPlay2 :: [(Domino, Int)] --If I had played a Domino card, I can lead by at least int points
firstPlay2 = zip domSet $ map f domSet 
    where f::Domino ->Int 
          f domino = scoreDiff (domSet\\[domino] ) P1 domino

--------------------------------------------------------------------------------------
-- Auxiliary function
--------------------------------------------------------------------------------------

--Get three functions of an element from a triple (a,b,c)
fst3::(a,b,c)->a 
fst3 (a,_,_) = a 

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b 

trd3::(a,b,c)->c 
trd3 (_,_,c) = c 
          
--Any card in the Hand that can be played results in a score above 61, that means bad Hand
badHand::DominoBoard->Hand->Bool 
badHand board hand = all ((>61).score) (ls' ++ rs') 
    where (ls,rs) = possPlays hand board 
          ls' = map (,L) ls 
          rs' = map (,R) rs
          score (domino,end) = scoreBoard $ fromJust $ playDom P1 domino board end --P1 is optional

--Try to place a card to the left side or right side to return the maximum score you can get. Zero points are awarded if no cards can be played.
playDomMaxScore::Player->DominoBoard->Domino->Int 
playDomMaxScore player board domino = max leftScore rightScore      
    where         
        leftScore = if canPlay domino L board then  
            scoreBoard $ fromJust $ playDom player domino board L
            else 0 
        rightScore = if canPlay domino R board then  
            scoreBoard $ fromJust $ playDom player domino board R 
            else 0


-- On a empty board, I play one card, and then the opponent plays the one that earns the most points 
-- (assuming that the opponent owns all of the sleep cards) to calculate the difference in scores
scoreDiff::OppoHand->Player->Domino->Int 
scoreDiff oppoHand me myDomino = myScore - oppoBestScore
    where oneDominoBoard = fromJust $ playDom me myDomino InitBoard L -- The card can be put on the board on both sides(L or R), so we can use fromJust
          myScore = scoreBoard oneDominoBoard
          opponent = if me == P1 then P2 else P1
          oppoBestScore =  if null oppoHand then 0 --When faced with a random opponent, a failed card guess leads to the belief that the opponent is out of cards
                else maximum $ map (playDomMaxScore opponent oneDominoBoard) oppoHand --On a board of only one card, the opponent plays one card and counts the opponent's score

--I play myDomino card first on the board and scored into myScore
--The opponent then plays one of his best cards and scores into oppoBestScore
--return myScore-oppoBestScore
--If the result is negative then myDomino card is not a good card
scoreDiff2 oppoHand board me (myDomino,end) = myScore - oppoBestScore 
    where myBoard = fromJust $ playDom me myDomino board end 
          myScore = scoreBoard myBoard 
          opponent = if me == P1 then P2 else P1 
          oppoBestScore = if null oppoHand then 0 --When faced with a random opponent, a failed card guess leads to the belief that the opponent is out of cards
              else maximum $ map (playDomMaxScore opponent myBoard) oppoHand 

--Given a board, calculate the total historical score of the two players from the history in the board (assuming the initial score is (0,0)).
sumScore :: DominoBoard->Scores 
sumScore (Board _ _ history) = sumScore' InitBoard hwe (0,0)
    where hwe = addEnd history 
          sumScore' _ [] scores = scores  
          sumScore' board (h:hs) (s1,s2) = sumScore' board' hs scores  
                where board' = fromJust $ playDom (snd3 h) (fst3 h) board (trd3 h)
                      score = scoreBoard board' 
                      scores = if snd3 h == P1 then (s1 + score,s2) else (s1,s2 + score)
          
--Find a card in the Hand that has a score of 61
arrive61::DominoBoard->Hand->Int->[(Domino,End)]
arrive61 _ [] _ = []
arrive61 board (d:ds) myScore = dl ++ dr ++ arrive61 board ds myScore
    -- where doms = scoreN board (61-myScore) -- bad
        --   dl = [(d,L) | (d,L) `elem` doms]
        --   dr = [(d,R) | (d,R) `elem` doms] 
    where l = canPlay d L board 
          r = canPlay d R board
          l_board = playDom P1 d board L --The history in the board can be ignored, so P1, P2, can be filled in randomly
          r_board = playDom P1 d board R --The history in the board can be ignored, so P1, P2, can be filled in randomly
          l_score = scoreBoard $ fromJust $ l_board 
          r_score = scoreBoard $ fromJust $ r_board 
          dl = [(d,L)| l && myScore + l_score == 61 ]
          dr = [(d,R)| r && myScore + r_score == 61 ] 

--after Me's Domino card played on Domino board, opponents have no cards to play
--Returns all the strokes that satisfy this condition [(Domino,End)]
tryBlock::Me->Domino->DominoBoard->OppoHand->[(Domino,End)]
tryBlock me domino board oppoHand = [(domino,L)| l && l_blocked] ++ [(domino,R)| r && r_blocked] 
    where l = canPlay domino L board  
          r = canPlay domino R board 
          boardl = fromJust $ playDom me domino board L 
          boardr = fromJust $ playDom me domino board R 
          l_blocked = blocked oppoHand boardl 
          r_blocked = blocked oppoHand boardr 

--After Me's Domino card play on Domino board, the opponent cannot earn score no matter how they play (no cards can be played or more than 61 points).
--Returns all the strokes that satisfy this condition [(Domino,End)]
oppoZeroGain::OppoHand->Scores->DominoBoard->Me->Domino->[(Domino,End)]
oppoZeroGain oppoHand (s1,s2) board me domino = [(domino,L)| l && l_exceed] ++ [(domino,R)| r && r_exceed] 
    where l = canPlay domino L board  
          r = canPlay domino R board 
          boardl = fromJust $ playDom me domino board L 
          boardr = fromJust $ playDom me domino board R 
          l_exceed = all (blockOrExceed61 oppoScore boardl oppo) oppoHand 
          r_exceed = all (blockOrExceed61 oppoScore boardr oppo) oppoHand
          oppo = if me==P1 then P2 else P1  
          oppoScore = if oppo==P1 then s1 else s2

--After Me's Domino play on the Domino board, the opponent can score points, calculating the maximum points that the opponent can increase
--Return all my plays and calculate the score
oppoLowGain::OppoHand->Scores->DominoBoard->Me->Domino->[(Int,(Domino,End))]
oppoLowGain oppoHand (s1,s2) board me domino
    |null oppoHand = []
    |l&&r = if l_maxScore > r_maxScore then l_result else r_result
    |l = l_result
    |r = r_result 
    |otherwise = []
    where l = canPlay domino L board  
          r = canPlay domino R board 
          boardl = fromJust $ playDom me domino board L 
          boardr = fromJust $ playDom me domino board R 
          l_maxScore = maximum $ map (oppoGain oppoScore boardl oppo) oppoHand 
          r_maxScore = maximum $ map (oppoGain oppoScore boardr oppo) oppoHand
          l_result = [(l_maxScore,(domino,L))]
          r_result = [(r_maxScore,(domino,R))]
          oppo = if me==P1 then P2 else P1  
          oppoScore = if oppo==P1 then s1 else s2

--After Me's Domino plays on Domino board, the opponent scores exactly 61 points (must be avoided).
--Return all of my plays
helpOppoWin::OppoHand->Scores->DominoBoard->Me->Domino->[(Domino,End)]
helpOppoWin oppoHand (s1,s2) board me domino = [(domino,L)| l && l_win] ++ [(domino,R)| r && r_win] 
    where l = canPlay domino L board  
          r = canPlay domino R board 
          boardl = fromJust $ playDom me domino board L 
          boardr = fromJust $ playDom me domino board R 
          l_win = not $ null $ arrive61 boardl oppoHand oppoScore 
          r_win = not $ null $ arrive61 boardr oppoHand oppoScore
          oppo = if me==P1 then P2 else P1  
          oppoScore = if oppo==P1 then s1 else s2

--[(Domino,End)] is the cards that can block an opponent after I play, 
-- and I need to sort them so that the card that will benefit me the most from my next play comes first.
sortByMyNextScore::Int->DominoBoard->Me->[(Domino,End)]->[(Domino,End)]
sortByMyNextScore myScore board me myOptions 
    = map snd $ reverse $ sortBy f $ map (nextBestScore myScore board me) myOptions
    where f x y = fst x `compare` fst y 
          nextBestScore myScore board me (domino,end) = (bestScore,(domino,end))
                where board' = fromJust $ playDom me domino board end
                      myScore' = myScore + scoreBoard board'
                      remainDominos = map fst myOptions \\ [domino]
                      bestScore = maximum $ map (nextScore myScore' board' me) remainDominos--bug maximum empty
                      nextScore myScore2 board2 me2 domino2 
                          | l && r = max l_score' r_score'
                          | l = l_score' 
                          | r = r_score' 
                          | otherwise = myScore2 
                          where l = canPlay domino2 L board2 
                                r = canPlay domino2 R board2 
                                l_score = scoreBoard $ fromJust $ playDom me2 domino2 board2 L 
                                r_score = scoreBoard $ fromJust $ playDom me2 domino2 board2 R 
                                l_score' = if myScore2 + l_score > 61 then myScore2 else myScore2 + l_score 
                                r_score' = if myScore2 + r_score > 61 then myScore2 else myScore2 + r_score 

oppoGain::Int->DominoBoard->Opponent->Domino->Int 
oppoGain oppoScore board oppo domino 
    | l && r = max l_gain r_gain 
    | l = l_gain 
    | r = r_gain 
    | otherwise = 0
    where l = canPlay domino L board 
          r = canPlay domino R board  
          l_score = scoreBoard $ fromJust $ playDom oppo domino board L
          r_score = scoreBoard $ fromJust $ playDom oppo domino board R 
          l_gain = if oppoScore + l_score > 61 then 0 else l_score
          r_gain = if oppoScore + r_score > 61 then 0 else r_score 


-- Returns true if Player play a Domino card and score exceeds 61 on the Domino board
-- Returns true if Domino card cannot play
blockOrExceed61 :: Int -> DominoBoard -> Player -> Domino -> Bool
blockOrExceed61 score board player domino
        = (not l && not r) || 
          (not r && l && l_exceed ) || 
          (not l && r && r_exceed ) || 
          (l && r && l_exceed && r_exceed)
    where l = canPlay domino L board
          r = canPlay domino R board 
          l_score = scoreBoard $ fromJust $ playDom player domino board L 
          r_score = scoreBoard $ fromJust $ playDom player domino board R
          l_exceed = score + l_score > 61 
          r_exceed = score + r_score > 61

--for test
randomTactic :: Tactic
randomTactic oppoHand (domino:rest) board player scores
        | leftBoard /= Nothing = [(domino,L)]
        | rightBoard /= Nothing = [(domino,R)]
        | otherwise = randomTactic oppoHand rest board player scores
        where
        leftBoard = playDom player domino board L    -- try playing it on the left end
        rightBoard = playDom player domino board R   -- try playing it on the right end