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
import Rummy.Rules


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
-- which action to call, which is Gin, Knock or Drop. The player will make
-- decision based on the memory of previous progress saved for each turn. 
-- For more information regarding the strategy used, please refer to the
-- report.
playCard :: PlayFunc
playCard picked score memory hand = 
    (action, writeMeminPlay memory (cardToDiscard action) deadwood)
    where 
        deadwood = deadwoods (picked:hand)
        turn = getTurnMem memory               -- game turn from memory 
        oppScore = snd score                   -- opponent's score from memory
        discards = getCardsMem allDiscardPar memory -- discarded cards from memory
        opicks = getCardsMem oppPickPar memory -- opponent's picked cards from memory
        action = 
            chooseAction deadwood turn oppScore discards opicks picked hand
            -- action chosen by the player


-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand. This is the function where the player 
-- arrange all the Card in hand to Set, Straight and Deadwood respectively in order 
-- to make melds. Deadwood is obtained from the memory to increase efficiency.
makeMelds :: MeldFunc
makeMelds _ mem hand = toMelds (hand \\ deadwood) ++ (Deadwood <$> deadwood)
    where
        deadwood = getCardsMem deadwoodPar mem



-- Helper functions for pickCard function, might also be reused for other 
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
possibleStraight :: Suit -> [Card] -> [[Card]]
possibleStraight suit cards = filter hasPossibleStraight $ 
    subsequences sortedCards where
    sortedCards = sort (cardsWithSuit suit cards)

-- | Check if adding the card to cards will form more possible Straight.
toPickDiscardForStraight :: Card -> [Card] -> Bool
toPickDiscardForStraight card cards = 
    length (possibleStraightWithNewCard card cards) > 
    length (possibleStraight (getSuit card) cards)

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

-- | Check whether the game is in its next round.
isNextRound :: Int ->  Int -> Bool
isNextRound prev curr = prev /= curr

-- | Convert Score to Int.
readScore :: Score -> Int
readScore s = (getFirstFromMaybe . readInt) (show s)


-- Helper functions for playCard function, might also be reused for other 
-- functions.

-- | Check whether a list of Card is possible to form Straight.
checkStraightInCards :: ([Card] -> [[Card]]) -> [Card] -> [Card]
checkStraightInCards hasStraight cs
    | not (null (hasStraight cs)) = longestStr ++ 
        checkStraightInCards hasStraight unmatched
    | otherwise = []
    where
        longestStr = longest (hasStraight cs)
        unmatched = [j | j <- cs, j `notElem` longestStr]

-- | Return a list of cards which is possible to form Straight for every 
-- Suit.
cardsInStraight :: [Card] -> [Card]
cardsInStraight fullCards = 
    checkStraightInCards (possibleStraight Spade) fullCards ++ 
    checkStraightInCards (possibleStraight Club) cardsNoSpade ++ 
    checkStraightInCards (possibleStraight Diamond) cardsNoClub ++ 
    checkStraightInCards (possibleStraight Heart) cardsNoDiamond
    where
        cardsNoSpade = fullCards \\ 
            checkStraightInCards (possibleStraight Spade) fullCards
        cardsNoClub = cardsNoSpade \\ 
            checkStraightInCards (possibleStraight Club) cardsNoSpade
        cardsNoDiamond = cardsNoClub \\ 
            checkStraightInCards (possibleStraight Diamond) cardsNoClub


-- | Return a list of cards with all possible Straights and sort them in 
-- the form of Straight 3, 4 and 5 in different lists.
cardInStraightList :: [Card] -> [[Card]]
cardInStraightList [] = []
cardInStraightList cs
    | ((length sorted >= 3 && length sorted <= 5 )|| length sorted >= 8) 
        && isConsecutive sublist5 = 
            sublist5 : cardInStraightList (sorted \\ sublist5)
    | (length sorted == 7 || length sorted == 8 || length sorted == 10) 
        && isConsecutive sublist4 = 
           sublist4 : cardInStraightList (sorted \\ sublist4)
    | isConsecutive sublist3 = sublist3 : cardInStraightList (sorted \\ sublist3)
    | otherwise = [head sorted] : cardInStraightList (sorted \\ [head sorted])
    where
        sorted = sort cs
        sublist3 = take 3 sorted
        sublist4 = take 4 sorted
        sublist5 = take 5 sorted

-- | Sort a list of Cards and divide them into different lists based on their 
-- respective Suit. Cards of the same Suit will be placed in the same list.
sortStraights :: [Card] -> [[Card]]
sortStraights [] = []
sortStraights cs = filteredCardOfSameSuit : sortStraights rest
    where
        filteredCardOfSameSuit = cardsWithSuit (getSuit (head cs)) cs
        rest = cs \\ existIn filteredCardOfSameSuit cs


-- | Return a list of Cards which can form Set.
cardsInSet :: Int -> [Card] -> [Card]
cardsInSet num cs = sort $ uniq [i | i <- cs, countSet (getRank i) cs >= num]

-- | Count the number of Cards which can form Set based on the Rank specified.
countSet :: Rank -> [Card] -> Int
countSet rank = countFiltered (\x -> getRank x == rank)


-- Helper functions for writing and updating memory in playCard

-- | Write memory and store the current progress as a string when playCard function 
-- is called. The memory where it saves all the known discarded cards and the 
-- deadwood in the current turn is updated in this function.

writeMeminPlay :: String -> Card -> [Card] -> String
writeMeminPlay mem toDiscard deadwood = 
    frontMemStr ++ updatedDiscards ++ " " ++ picked ++ " " 
        ++ cardListToString (deadwood\\[toDiscard])
    where 
        discards = getCardsMem allDiscardPar mem
        frontMemStr = toString (getTurnMem mem) ++ " " 
            ++ toString (getScoreMem mem) ++ " "
        picked = cardListToString (getCardsMem oppPickPar mem)
        updatedDiscards = cardListToString (toDiscard:discards)

-- | Choose an Action, including Gin, Knock or Drop based on the strategy and 
-- decision made by the player.
chooseAction :: [Card] -> Int -> Int -> [Card] -> [Card] -> Card -> 
    [Card] -> Action
chooseAction deadwood turnParser oppscore discards opicks recentPick hand
    -- Choose Drop, play the Card with the highest value.
    | null nopick = Action Drop (maximum hand)

    -- Choose Gin, when one deadwood is left and it is not the first turn.
    | turnParser /= 1 && length deadwood == 1 && head deadwood /= recentPick = 
        Action Gin (head deadwood)

    -- Choose Knock, when the value of deadwood is less than 10 and not the
    -- first turn.
    | turnParser /= 1 && countCardValue deadwood < 10 && not (null nopick) = 
        Action Knock (maximum nopick)

    -- Choose Drop if none of the conditions above fits.
    | otherwise =
        Action Drop (chooseDiscard nopick turnParser oppscore discards opicks)
   
    where
        -- a variable where last drawn card is excluded as it is not allowed to
        -- play the last drawn card
        nopick = deadwood \\ [recentPick] 


-- | Count value of Cards.
countCardValue :: [Card] -> Int
countCardValue =  foldr ((+) . toPoints) 0

-- | Filter a list of cards without same and consecutive rank as the Card opick 
-- (opponent's most recent pick).
compareOppPick :: Foldable t => Card -> t Card -> [Card]
compareOppPick opick = foldr (\x xs -> 
    if notSameRank opick x && notConsecutiveRank opick x then x:xs else xs) []
    where
        notSameRank a b = getRank a /= getRank b
        notConsecutiveRank a b = 
            abs (fromEnum (getRank a) - fromEnum (getRank b)) /= 1


-- | Compare two list of Cards and check whether there is possible match to form melds.
-- Then, priority is given to certain Card to be discarded based on strategy used.
comparePossibleMeld :: [Card] -> [Card] -> Int -> Card
comparePossibleMeld deadwood discards turnParser
    -- If there is possible match, prioritise returning (discarding) the Card with the 
    -- highest value from the possible matches .
    | not (null possibleMeld) && not (null possibleMatchDeadwood) = 
        maximum possibleMatchDeadwood
    
    -- If there is no possible match and the game is currently at the first two turn,
    -- check whether the player owns Card with Rank Ace or Two, and prioritise discarding
    -- it.
    | null possibleMeld && turnParser <= 2 && (getRank . minimum) deadwood < Three = 
        minimum deadwood

    -- If none of the condition above is met, prioritse discarding the Card with the 
    -- highest value from deadwood.
    | otherwise = maximum deadwood
        where
            possibleMeld = melds (deadwood ++ discards)
            possibleMatchDeadwood = existIn deadwood possibleMeld


-- | Choose the discarded card from the current Cards in hand. The function compareOppPick
-- and comparePossibleMeld helps to filter and implement the strategy used.
-- For more information on the strategy used, please refer to the report.
chooseDiscard :: [Card] -> Int -> Int -> [Card] -> [Card] -> Card
chooseDiscard deadwood turnParser oppscore discards opicks
    | oppscore >= 75 = maximum deadwood
    | null opicks || null filtered = comparePossibleMeld deadwood discards turnParser
    | length filtered > 1 = comparePossibleMeld filtered discards turnParser
    | otherwise = head filtered -- here output of compareOppPick only has 1 element
    where 
        filtered = compareOppPick (head opicks) deadwood

-- | Return only the melds from a list of Card, excluding deadwood.
melds :: [Card] -> [Card]
melds cs = set ++ concat straight
    where 
        set = cardsInSet 3 cs
        withoutSet = cardsInStraight (cs \\ set)
        straight = concat $ filter (\x -> length x > 1) <$> 
            (cardInStraightList <$> sortStraights withoutSet)

-- | Return only the deadwood from a list of Card.
deadwoods :: [Card] -> [Card]
deadwoods cs = sort (cs \\ melds cs)

-- | Arrange and convert type Card to type Meld to make melds at the end of the
-- game.
toMelds :: [Card] -> [Meld]
toMelds cs = makeSetMeld cs1 ++ makeStraightMeld cs2 ++ (Deadwood <$> cs3)
    where
        cs1 = cardsInSet 3 cs
        cs2 = cardsInStraight (cs \\ cs1)
        cs3 = cs \\ (cs1 ++ cs2)
   
-- | Sort the Sets with the same rank in the same list, for all Ranks.
sortSets :: [Card] -> [[Card]]
sortSets [] = []
sortSets cs = filteredCardOfSameRank : sortSets rest
    where 
        filteredCardOfSameRank = cardsWithRank (getRank (head cs)) cs
        rest = cs \\ existIn filteredCardOfSameRank cs

-- | Convert a list of Card to a list of Straights for all Straight
-- combination in the Card list.
makeStraightMeld :: [Card] -> [Meld]
makeStraightMeld [] = []
makeStraightMeld cs = concat $ makeStraight <$> 
    concat (cardInStraightList <$> sortStraights cs)

-- | -- | Convert a list of Card to a list of Sets for all Set
-- combination in the Card list.
makeSetMeld :: [Card] -> [Meld]
makeSetMeld [] = []
makeSetMeld cs = concat (makeSets <$> sortSets cs)

-- | Convert a list of Set combination of Card type in specific
-- sequence to the type Set 3 and Set 4 respectively.
makeSets :: [Card] -> [Meld]
makeSets cs = case length cs of
    3 -> [Set3 (head sorted) (sorted!!1) (sorted!!2)]
    4 -> [Set4 (head sorted) (sorted!!1) (sorted!!2) (sorted!!3)]
    _ -> []
    where
        sorted = sort cs

-- | Convert a list of Straight combination of Card type in specific
-- sequence to the type Straight 3, Straight 4 and Straight 5 
-- respectively. If the length of the list of Card arranged is one,
-- the Card is a Deadwood.
makeStraight :: [Card] -> [Meld]
makeStraight cs = case length cs of
    1 -> [Deadwood (head sorted)]
    3 -> [Straight3 (head sorted) (sorted!!1) (sorted!!2)]
    4 -> [Straight4 (head sorted) (sorted!!1) (sorted!!2) (sorted!!3)]
    5 -> [Straight5 (head sorted) (sorted!!1) (sorted!!2) (sorted!!3) (sorted!!4)]
    _ -> []
    where
        sorted = sort cs      


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