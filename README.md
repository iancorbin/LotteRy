# LotteRy
Lottery ball game simulations in R.

Using WA Lottery games to simulate winnings over any number of games (or trials) played. The simulations can be easily modified to adapt to any state lottery game where a number is drawn and order of the numbers has no impact on the outcome of the game. 

Each game requires five inputs:
1. Cost, in dollars. Example: ```cost <- 2``` would represent a cost of $2 per play.
2. Number of tickets to purchase. Example: ```trials <- 10655``` represents the number of tickets to buy.
3. Lowest number available to pick. Example: ```numlow <- 1``` would represent the number 1 being the lowest possible number to choose.
4. Highest number available to pick. Example: ```numhigh <- 24``` would represent the number 24 being the highest possible number to choose. Combining numlow and numhigh gives us our possible range of choice. In this case, numbers between 1 and 24.
5. Number of balls you are required to pick. Example: ```numpick <- 4``` would represent a a requirement to pick 4 numbers. Each game will be different.
