-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
pickCard = undefined

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard = undefined

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds = undefined
