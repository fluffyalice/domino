# Haskell - Domino games design and experiment

## Players:

1. cleverPlayer: CleverPlayer combines all of the tactics , and this is the main player which gonna play the game.

2. idiotGreedyPlayer: the idiot greedy strategy: Always plays the highest card, regardless of whether the opponent gets a higher score on my hand.

3. smartGreedyPlayer: smart and greedy strategy: Always play the cards that have the biggest margin of victory over my opponent (i.e., consider whether my opponent can take advantage of my cards to score more points).

4. badHandPlayer: A bad card is one on which no score can be scored (each card is played for more than 61 points). The main implement is using Block Opponent Strategy. Enter the next round in order to achieve the purpose of changing cards.

## Tactics:

1. verySmartGreedy: it has the same ability of the smart and greedy strategy. The difference is that it can also try to filter out the cards that help the opponent score 61 points after I play cards. Notice: This is a real fight strategy, the other two greedy strategies are not strong enough.
2. opponentLowGain: a strategy of returning a low score to an opponent. The data returned is a sorted list, and the strategy at the head of the list gives the opponent the lowest score.
3. opponentZeroGain: a strategy that prevents an opponent from scoring.
4. doubleBlock: a strategy to block both players.
5. blockMyself: a strategy that block myself.
6. blockOpponent: a strategy that blocks the opponent.
7. randomTactic(for test)

## Functions:

1. badHand: any card in the Hand that can be played results in a score above 61, that means bad Hand.

2. playDomMaxScore: try to place a card to the left side or right side to return the maximum score you can get. Zero points are awarded if no cards can be played.

3. scoreDiff: on a empty board, I play one card, and then the opponent plays the one that earns the most points. (assuming that the opponent owns all of the sleep cards) to calculate the difference in scores.

4. scoreDiff2: I play myDomino card first on the board and scored into myScore. The opponent then plays one of his best cards and scores into oppoBestScore, return myScore-oppoBestScore. If the result is negative then myDomino card is not a good card.

5. sumScore: given a board, calculate the total historical score of the two players from the history in the board (assuming the initial score is (0,0)).

6. arrive61: find a card in the Hand that has a score of 61.

7. tryBlock: after Me's Domino card played on Domino board, opponents have no cards to play, returns all the strokes that satisfy this condition [(Domino,End)].

8. oppoZeroGain: after Me's Domino card play on Domino board, the opponent cannot earn score no matter how they play (no cards can be played or more than 61 points), returns all the strokes that satisfy this condition [(Domino,End)].

9. oppoLowGain: after Me's Domino play on the Domino board, the opponent can score points, calculating the maximum points that the opponent can increase, return all my plays and calculate the score.

10. helpOppoWin: after Me's Domino plays on Domino board, the opponent scores exactly 61 points (must be avoided), return all of my plays.

11. sortByMyNextScore: [(Domino,End)] is the cards that can block an opponent after I play, and I need to sort them so that the card that will benefit me the most from my next play comes first.

12. blockOrExceed61: Returns true if Player play a Domino card and score exceeds 61 on the Domino board. Returns true if Domino card cannot play.

13. oppoGain: calculate the score opponent get, return the score.

## Algorithm: 

1. guessOppoHand: guess the opponent's cards, return true means the starting score is (0,0).

2. historyRecur: repeat history, brushing out cards the opponent may not have (if Opponent have the card, they will play them in history for a higher score). If the board is empty (InitBoard). If the opponent doesn't play the (5,4) card that the code suggests, can I guess he doesn't have this card? What about other case? Answer: according to the function firstPlay2, (5,4) is the only suggested card, and any other card that is played is likely to give the opponent a higher score than me.

3. addEnd: add End(L or R) to the elements in the history and sort MoveNum from small to large. The purpose is to facilitate the use of historyRecur.

4. firstPlay: the starting score is (0,0), and which card is the best when it's my turn to play the first card? Answer: Pick out the card from my hand that has scored the biggest difference between my opponent's next score.

5. firstPlay2: this function does not participate in the play, just analysis before the play of cards. After the analysis of this function, (5,4) is the only card that my opponent cannot play with a higher score after I play on the board (suppose I play a card and the opponent can play any card other than this card).


The system determines that I have a card to play, and when it is my turn to play, my strategy is:

	If it's empty and the score is (0,0){
		Use the empty board strategy.
	} Otherwise {
		Try to score 61 points {
			If I have a card with 61 points {
				play cards
	        }Otherwise, if all my cards give me more than 61 points{
	
				That means all I have are bad cards, and I need to change my cards.
	
				In order to changing cards, I use the -doubleBlock- strategy to make sure the score of both players does not change when players go to the next round.
	
				Block both players strategy (doubleBlock) (doubleBlock = blockOpponent + blockMyself) is to try to play a card that can block both players. If the strategy fails, try use -blockOpponent- strategy.
	
				Block opponent strategy (blockOpponent) is to play a card that will block the opponent. if the strategy fails, try use -opponentLowGain- strategy.
	
				Give opponent low score strategy (opponentLowGain) is when I played a card, the opponent will win score, but the score is the lowest in all possible score.
	
				Give opponent player zero score strategy (opponentZeroGain) is a special case which will give opponent zero points. When I play a card, let all the cards in opponent's into bad cards (that means he can get high scores but the score will be more than 61 points).
	
			} Otherwise {
	
				Both -blockOpponent- strategy and -verySmartGreedyPlayer- strategy are calculated, where {
					Definition of -blockOpponent- strategy {
	
						Definition: A block card that means leaves the opponent with no cards to play
	
						Find a block card from my hand. If I find one block card{
							play
						} otherwise if more than one block cards{
							Take one of my block cards which can make the score close to or equal to 61 after my next play.
						} otherwise if {
							-blockOpponent- strategy failed, change the strategy to -verySmartGreedyPlayer- strategy.
						}
					}
				}
				According to the above calculation results:
	
				If neither strategy works, then used randomPlayer to play a random card.
	
				If only one strategy works , use that strategy.
	
				If both strategies work {
					Compare the score with the opponent, if the opponent has a higher score {
						Use -blockOpponent- strategy.
					} Otherwise {
						use -verySmartGreedyPlayer- strategy.
					}
				}
			}
		}
	}


# Experimental report

## The experimental method:

Using domsMatch P1, P2, 1000 31, P1 and P2 can be: randomPlayer, idiotGreedyPlayer, smartGreedyPlayer, cleverPlayer.  The first three types of players can run domsMatch directly for testing. CleverPlayer tests it in a special way.

## The experimental results：

### i. Direct play:

RandomPlayer vs randomPlayer = (506,494)
IdiotGreedyPlayer vs randomPlayer = (973,27)
SmartGreedyPlayer vs idiotGreedyPlayer = (562,438)
CleverPlayer vs smartGreedyPlayer = (620,380)

(During the development process, there was also a verySmartGreedyPlayer which was obviously better than the smartGreedyPlayer when tested, but was later renamed verySmartGreedy Tactic, so they couldn't be tested.)

### ii. Strategy:

CleverPlayer uses a variety of strategies, including:
1. Guessing the opponent's cards.
2. Blocking the opponent.
3. Blocking myself.
4. Blocking both players.
5. Filter out the cards that help the opponent score 61 points after I play cards.
6. Trying to avoid the opponent scoring high points.
7. When blocking an opponent, consider choosing a card which will give me the highest score next move.

To demonstrate that each strategy improves the win ratio, first run cleverPlayer vs. smartGreedyPlayer and get (620,380)
Then replace the key code for one certain strategy with a random strategy and run cleverPlayer vs. smartGreedyPlayer again.
If the running results are worse than (620,380), it proves that each strategy improves the winning rate.

### iii. Here are the test results:

```
Cancel the opening strategy                                                                (620,380)
Eliminate preferential play for 61 points                                                  (545,455)
Random strategy instead of bad card strategy                                               (620,380)
Random strategy instead of super-smart greedy strategy                                     (33,967)
Random Strategy instead of blocking strategy                                               (612,388)
Does not select my next highest score card while blocking the opponent                     (618,382)
Guessing the opponent's cards does not assume that the opponent is using a greedy strategy (578,422)
Do not take advantage of an opponent's block when guessing his cards                       (615,385)
Don't guess, I always assume that my opponent has 9+10 cards                               (527,473)
```

## Conclusion:

Most strategies raise the winning interest rate, with only two exceptions. The two exceptions are the opening strategy and the bad card strategy.
