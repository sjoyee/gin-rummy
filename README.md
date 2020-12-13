# Gin Rummy
This is an individual academic project for the course Programming Paradigms. I worked on Player.hs (in submission folder) to implement a player for the game of Gin Rummy, which is a two-player strategy card game. This assignment is fully implemented in Haskell, a purely functional programming language. 

Base code is provided by Professor Tim Dwyer. 


# Player, Memory and Strategy
I chose to implement the player with heuristic approach where it saves additional information about the game in memory and utilises the memory to enhance decision at each turn. More information on the strategy and memory usage were written in the report, which is a PDF file included in the submission folder.

# Rules
There are some key differences in the variant of Gin Rummy we used in this [assignment](https://fit2102.monash/resources/assignment.html).

- The non-dealer does not get to take the up-card, the game starts after dealing. 

- There is non laying-off, we only count the melds formed in your own hand.

- You cannot discard the card you just drew.

- There is no Big Gin, you always have to discard at the end of your turn.

- If no player takes an action before the stock runs out, the last player to draw is considered to have Knocked.

A round proceeds as below:
1. A dealer is chosen at random; this player will be last to go.

2. Each player is dealt ten cards; these form a hand.

3. The first card after dealing two hands is revealed and put face-side up, this forms the
discard; the rest of the cards form the stock.

4. In turn, each player then decides whether they want to pick the (visible) card from the
discard, or the (hidden) top card from the stock.

5. To end their turn, players will have to discard a card from their hand and announce if they
want to end the game.

At the end of their turn, players thus discard a card and have three choices:

1. Call Gin, which means that they managed to fit all ten cards in their hand into meld. Calling Gin awards bonus points.

2. Knock, which means that, although they did not manage to fit all ten cards into melds, they believe to have a hand of lower value that their opponent’s. You can only Knock if your deadwood’s total value is less than 10.

3. Discard, which means that they do not want to end the game.

# Forming melds

The core mechanic of Gin Rummy is to fit cards into melds – think poker combinations. In our variant of Gin Rummy, we will use three types of melds:

- Straight: a combination of three to five cards of the same suit with consecutive numbers. For example: A♠ 2♠ 3♠.

- Set: a combination of three or four cards with the same rank in different suits. For example: 8♥ 8♦ 8♠.

- Deadwood: any card which does not fit into a meld.

Click [here](https://fit2102.monash/resources/assignment.html) for more information.


# How to run the game
1. Run ```stack build``` to compile the code.

2. Run ```stack exec staticgame``` to execute the code.

The base code will set up a tournament with two instances of my implementation of player playing against each other. The results will be displayed on the terminal.

# Plagiarism Disclaimer
Plagiarism is a serious offence and will result in a breach in the Academic Integrity Policy. The author @sjoyee will not be responsible for any breaches in academic integrity. For more information, please visit [here](https://www.monash.edu/students/admin/policies/academic-integrity).
