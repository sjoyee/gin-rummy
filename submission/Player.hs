-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Parser.Instances
import Data.Ord
import Data.List
import Data.Char


-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from. Based on the strategy used, player will try forming
-- possible Set or Straight between the card on top of the Discard pile with the 
-- cards in hand. If there is at least one possible match, player will draw from 
-- Discard, otherwise from Stock. Memory of current turn, scores, opponent's 
-- drawn card if they drew from Discard (visible to all players), will be stored
-- as a string to be passed to the next turn or action. For more information about
-- the strategy used and the memory parsing, please refer to the report.
pickCard :: ActionFunc
pickCard card scores mem oppDraw hand 
    | pickDiscard card hand = (Discard, memoryD)
    | otherwise = (Stock, memoryS)
    where
        memoryD = writeToMemD card mem scores oppDraw
        memoryS = writeToMemS card mem scores oppDraw

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard = undefined

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds = undefined



-- Helper functions for pickCard function, might also be reused in other 
-- functions.

-- | Get the Suit of Card by pattern matching.
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- | Get the Rank of Card by pattern matching.
getRank :: Card -> Rank
getRank (Card _ rank) = rank

-- | Return a list of Card with the same suit specified by the argument.
cardsWithSuit :: Suit -> [Card] -> [Card]
cardsWithSuit suit = filter (\x -> getSuit x == suit)

-- | Return a list of Card with the same rank specified by the argument.
cardsWithRank :: Rank -> [Card] -> [Card]
cardsWithRank rank = filter (\x -> getRank x == rank)

-- | Get the card to be discarded from Action by pattern matching.
cardToDiscard :: Action -> Card
cardToDiscard (Action _ c) = c

-- | Check if card can form Set with hands.
hasPossibleSet :: Card -> [Card] -> Bool
hasPossibleSet card hand = length (cardsWithRank (getRank card) hand) >= 2

-- | Check if cards is consecutive with length larger than 3.
hasPossibleStraight :: [Card] -> Bool
hasPossibleStraight cards = (length (take 3 cards) >= 3) && isConsecutive cards

-- | Check if cards is consecutive in rank.
isConsecutive :: [Card] -> Bool
isConsecutive = consecutive . ranked where
    ranked = sort . map getRank 
    consecutive l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1)

-- | Check if card can from Straight with cards.
possibleStraightWithNewCard :: Card -> [Card] -> [[Card]]
possibleStraightWithNewCard card cards = filter hasPossibleStraight $ 
    subsequences sortedCards where
    sortedCards = sort [card] ++ cardsWithSuit (getSuit card) cards

-- | Check if cards can form Straight among themselves.
possibleStraightWithoutNewCard :: Suit -> [Card] -> [[Card]]
possibleStraightWithoutNewCard suit cards = filter hasPossibleStraight $ 
    subsequences sortedCards where
    sortedCards = sort (cardsWithSuit suit cards)

-- | Check if adding the card to cards will form more possible Straight.
toPickDiscardForStraight :: Card -> [Card] -> Bool
toPickDiscardForStraight card cards = 
    length (possibleStraightWithNewCard card cards) > 
    length (possibleStraightWithoutNewCard (getSuit card) cards)

-- | Check if player should draw from Discard by forming possible melds with 
-- the card and not with the card.
pickDiscard :: Card -> [Card] -> Bool
pickDiscard card hand = hasPossibleSet card hand || 
    toPickDiscardForStraight card hand


-- Helper functions for writing and updating memory in pickCard

-- | Store additional information in memory and updating it in the case when
-- player draw from Stock. Drawing from different piles have different ways to
-- handle the memory string (please read the report for more information).
writeToMemS :: Card -> Maybe String -> (Score, Score) -> Maybe Draw -> String
writeToMemS card mem (playCurr, oppCurr) oppDraw
    | isNothing mem = "1 " ++ toString currScores ++ " " ++ 
        cardListToString (handleDiscardS mem 1 card []) ++  "    "
    | otherwise = toString updatedTurn ++ " "  ++ 
        toString currScores ++ " "  ++  updatedDiscards ++ " " ++ 
        updatedOppPicks ++ "  "
    where
        currScores = (readScore playCurr, readScore oppCurr)
        memstr = fromJust mem
        updatedTurn = 
            countTurn (getTurnMem memstr) (getScoreMem memstr) currScores
        discards = getCardsMem allDiscardPar memstr
        picked = getCardsMem oppPickPar memstr
        updatedDiscards = 
            cardListToString (handleDiscardS mem updatedTurn card discards)
        updatedOppPicks = 
            cardListToString (handleOppPick picked discards oppDraw)

-- | Store additional information in memory and updating it in the case when
-- player draw from Discard. Drawing from different piles have different ways to
-- handle the memory string (please read the report for more information).
writeToMemD :: Card -> Maybe String -> (Score, Score) -> Maybe Draw -> String
writeToMemD card mem (playCurr, oppCurr) oppDraw
    | isNothing mem = "1 " ++ toString currScores ++ " " ++ 
        cardListToString (handleDiscardD mem 1 oppDraw card []) ++  "    "
    | otherwise = toString updatedTurn ++ " " ++ 
        toString currScores ++ " " ++ updatedDiscards ++ " " ++ 
        updatedOppPicks ++ "  "
    where
        currScores = (readScore playCurr, readScore oppCurr)
        memstr = fromJust mem
        updatedTurn = 
            countTurn (getTurnMem memstr) (getScoreMem memstr) currScores
        discards = getCardsMem allDiscardPar memstr
        picked = getCardsMem oppPickPar memstr
        updatedDiscards = 
            cardListToString (handleDiscardD mem updatedTurn oppDraw card discards)
        updatedOppPicks = 
            cardListToString (handleOppPick picked discards oppDraw)

-- | Handle discarded card pile in memory if player draw from Stock pile.
handleDiscardS :: Maybe String -> Int -> Card -> [Card] -> [Card]
handleDiscardS mem turn card discards
    | isJust mem && turn == 1 && not (null discards) = 
        handleDiscardS mem turn card []
    | otherwise = card : discards

-- | Handle discarded card pile in memory if player draw from Discard pile.
handleDiscardD :: Maybe String -> Int -> Maybe Draw -> Card -> [Card] -> [Card]
handleDiscardD mem turn oppDraw card discards
    | isNothing mem && isNothing oppDraw = []
    | isJust mem && turn == 1 && not (null discards) = 
        handleDiscardD mem turn oppDraw card [] 
    | otherwise = discards

-- | Handle opponent's picked card pile in memory.
handleOppPick :: [Card] -> [Card] -> Maybe Draw -> [Card]
handleOppPick picked discards oppDraw
    | isDrawDiscard oppDraw && discards /= [] = head discards : picked 
    | otherwise = picked

-- | Update the game turn.
countTurn :: Int -> (Int, Int) -> (Int, Int) -> Int
countTurn t (playPrev,oppPrev) (playCurr, oppCurr)
    | isNextRound oppPrev oppCurr || isNextRound playPrev playCurr = 1 
    | otherwise = t + 1

-- | Check if Draw is Discard by pattern matching.
isDrawDiscard :: Maybe Draw -> Bool
isDrawDiscard (Just Discard) = True
isDrawDiscard (Just Stock) = False
isDrawDiscard Nothing = False

-- | Check if it is next round.
isNextRound :: Int ->  Int -> Bool
isNextRound prev curr = prev /= curr

-- | Convert Score to Int.
readScore :: Score -> Int
readScore s = (getFirstFromMaybe . readInt) (show s)


-- Show instances
-- | Show instance for Suit for Card
instance Show Suit where
    show Spade = "S"
    show Club = "C"
    show Diamond = "D"
    show Heart = "H"

-- | Show instance for Rank for Card
instance Show Rank where
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"
    show Ten = "T"
    show rank = show $ fromEnum rank + 1    -- Rank Three to Rank Nine

-- | Show instance for Card, when print out, for example, it is in the form of S2 
-- for Card Spade Two.
instance Show Card where
    show (Card s r) = show s ++ show r

-- | Helper functions for parsing memory
list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = do
        p' <- p
        p'' <- list p
        pure (p' : p'')

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P k 
    where 
        k (c:cs) = if f c then Result cs c else Error (UnexpectedChar c)
        k "" = Error UnexpectedEof

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = list digit

space :: Parser Char
space = satisfy isSpace 

-- Parsers

-- Parser for Suit
-- | Parse suit of a Card.
spade :: Parser Suit
spade = is 'S' >> pure Spade

club :: Parser Suit
club = is 'C' >> pure Club

diamond :: Parser Suit
diamond = is 'D' >> pure Diamond

heart :: Parser Suit
heart = is 'H' >> pure Heart

suits :: Parser Suit
suits = spade ||| club ||| diamond ||| heart

-- Parser for Rank
-- | Parse rank of a Card.
ranks :: Parser Rank
ranks = (is 'K' >> pure King) ||| (is 'Q' >> pure Queen) ||| (is 'J' >> pure Jack) 
    ||| (is 'T' >> pure Ten) ||| (is '9' >> pure Nine) ||| (is '8' >> pure Eight) 
    ||| (is '7' >> pure Seven) ||| (is '6' >> pure Six) ||| (is '5' >> pure Five) 
    ||| (is '4' >> pure Four) ||| (is '3' >> pure Three) ||| (is '2' >> pure Two) 
    ||| (is 'A' >> pure Ace)

-- Parser for Memory
-- | Parse a Card, for example Card Spade Two.
cardPar :: Parser Card
cardPar = do
    s <- suits
    r <- ranks
    pure $ Card s r

-- | Parse a list of Card.
cardListPar :: Parser [Card]
cardListPar = list cardPar

-- | Parse a tuple of integers with purpose of storing scores.
scorePar :: Parser (Int, Int)
scorePar = do
    _ <- is '('
    a <- digits
    _ <- is ','
    b <- digits
    _ <- is ')'
    pure (stringtoInt a, stringtoInt b)

-- | Parse three card piles.
piles :: Parser ([Card], [Card], [Card])
piles = do
    a <- cardListPar
    _ <- space
    b <- cardListPar
    _ <- space
    c <- cardListPar
    pure (a,b,c)

-- | Parse the turn of the game, which is an integer.
turnPar :: Parser Int
turnPar = do stringtoInt <$> digits

-- | Parse all scores of the game from the memory string, which is a tuple of 
-- integers.
allScorePar :: Parser (Int, Int)
allScorePar = do
    _ <- turnPar 
    _ <- space
    s <- scorePar
    _ <- piles
    pure s

-- | Parse all discard piles from the memory string, which is a list of Card.
allDiscardPar :: Parser [Card]
allDiscardPar = do
    _ <- turnPar
    _ <- space
    _ <- scorePar
    _ <- space
    c <- cardListPar
    _ <- space
    _ <- cardListPar
    _ <- space
    _ <- cardListPar
    pure c ||| pure []


-- | Parse opponent's picked card pile from the memory string, which is a list
-- of Card.
oppPickPar :: Parser [Card]
oppPickPar = do
    _ <- turnPar 
    _ <- space
    _ <- scorePar
    _ <- space
    _ <- cardListPar
    _ <- space
    c <- cardListPar
    _ <- space
    _ <- cardListPar
    pure c ||| pure []

-- | Parse the Deadwood pile from the memory string, which is a list of Card
-- with no matched cards to form Set or Straight.
deadwoodPar :: Parser [Card]
deadwoodPar = do
    _ <- turnPar 
    _ <- space
    _ <- scorePar
    _ <- space
    _ <- cardListPar
    _ <- space
    _ <- cardListPar
    _ <- space
    c <- cardListPar
    pure c ||| pure []

-- | Get the memory from the parse result by pattern matching.
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "You should not do that!"

-- | Get the memory in list from the parse result by pattern matching.
getMemList :: ParseResult [a] -> [a]
getMemList (Result _ cs) = cs
getMemList (Error _) = []

-- | Return a list of Card from memory using respective parser to parse
-- the memory string.
getCardsMem :: Parser [Card] -> String -> [Card]
getCardsMem parser str = getMemList (parse parser str)

-- | Return a tuple of integers which is the scores using the score
-- parser to parse the memory string.
getScoreMem :: String -> (Int,Int)
getScoreMem str = getMem (parse allScorePar str)

-- | Return an integer which indicate the game turn using the turn
-- parser to parse the memory string.
getTurnMem :: String -> Int
getTurnMem str = getMem (parse turnPar str)

-- | Convert a list of Card to a String of Card list.
cardListToString :: [Card] -> String
cardListToString cs = concat $ toString <$> cs


-- Helper functions for general purpose

-- | Pattern match to return the String from Maybe String or else return 
-- an empty String.
fromJust :: Maybe String -> String
fromJust (Just i) = i
fromJust Nothing = ""

-- Pattern match to return the integer from Maybe (Int, a) else return 0
getFirstFromMaybe :: Maybe (Int, b) -> Int
getFirstFromMaybe (Just (i,_)) = i
getFirstFromMaybe Nothing = 0

-- | Convert integer in string to integer.
stringtoInt :: String -> Int
stringtoInt = getFirstFromMaybe . readInt

-- | Convert argument to string.
toString :: Show a => a -> String
toString = show

-- | Check if Maybe type is Nothing by pattern matching. 
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _)  = False

-- | Check if Maybe type is not Nothing by pattern matching. 
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- | Find the longest element in list.
longest :: [[a]] -> [a]
longest = maximumBy (comparing length)

-- | Count the length of list after filtered by f.
countFiltered :: (a -> Bool) -> [a] -> Int
countFiltered f = length . filter f 

-- | Return a list with no duplicated element.
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

-- | Return a list with elements that appear in both arguments (list).
existIn :: (Foldable t, Eq a) => t a -> [a] -> [a]
existIn checkFrom = filter (`elem` checkFrom)