# WA Lottery Hit 5 Game Simulation
# Author: Ian Corbin from Seattle, WA
# Web: http://www.iancorbin.com / Twitter: @iancorbin
# Game Rules: http://www.walottery.com/JackpotGames/Hit5.aspx

# set.seed(9999)  # Set seed if you want to reproduce the results

# Set game variables
cost <- 1         # Cost per play (in dollars)
trials <- 10000   # Number of tickets to purchase
numlow <- 1       # Lowest number available to pick
numhigh <- 39     # Highest number available to pick
numpick <- 5      # Number of balls available to pick


# Start the simulation (you don't have to modify anything below this line)
y <- replicate(trials,sample(numlow:numhigh,numpick,replace=FALSE))
dimnames(y) <- list(rownames(y,do.NULL=FALSE,prefix="N"),
                    colnames(y,do.NULL=FALSE,prefix="Combination"))
m <- t(y)
(m2 <- t(apply(m,1,sort)))

x <- replicate(1,sample(numlow:numhigh,numpick,replace=FALSE))
dimnames(x) <- list(rownames(x,do.NULL=FALSE,prefix=""),
                    colnames(x,do.NULL=FALSE,prefix="Winning Numbers"))
w <- t(x)
(w2 <- t(apply(w,1,sort)))

nfact <- factorial(numhigh)
cfact <- factorial(numpick)
combos <- nfact/(cfact*factorial(numhigh - numpick))

wincount <- rowSums(matrix(m %in% w, ncol=ncol(m)))
wincount
c("$0","$0","$1","$10","$100","$100000")[apply(m2,1,function(x) sum(x%in%w))+1]

win <- sum(c(0,0,1,10,100,100000)[apply(m2,1,function(x) sum(x%in%w))+1])
bet <- cost * trials
delta <- win - bet
winningpercentage <- (win - bet)/bet
paste("Won $",format(win, big.mark=","),sep="")
paste("Spent $",format(bet, big.mark=","),sep="")
paste("Delta $",format(delta, big.mark=","),sep="")
paste("Winning %",format(winningpercentage, big.mark=","),sep="")

matchzero <- length(which(wincount == 0)) 
matchone <- length(which(wincount == 1)) 
winnothing <- sum(matchzero + matchone)

matchtwo <- length(which(wincount == 2)) 
matchthree <- length(which(wincount == 3)) 
matchfour <- length(which(wincount == 4)) 
winjackpot <- length(which(wincount == 5)) 

winsomething <- length(which(wincount > 1)) 

paste("Won Something: ",format(winsomething, big.mark=","),sep="")

paste("$0: ",format(winnothing, big.mark=","),sep="")
paste("$1: ",format(matchtwo, big.mark=","),sep="")
paste("$10: ",format(matchthree, big.mark=","),sep="")
paste("$100: ",format(matchfour, big.mark=","),sep="")
paste("$100,000: ",format(winjackpot, big.mark=","),sep="")

paste("Possible Combos: ",format(combos, big.mark=",", scientific = FALSE),sep="")
paste("Jackpot Probability: ",format(1/combos, big.mark=",", scientific = FALSE),sep="")
paste("Jackpot Odds: 1:",format(combos, big.mark=",", scientific = FALSE),sep="")

hist(wincount,
     main="Match Count",
     xlab="Matches",
     ylab="Density",
     # border="blue",
     # col="green",
     xlim=c(0,5),
     ylim=c(0,1.0),
     las=1,
     breaks=5,
     prob=TRUE)

# Print data frame of matches
wins<-c(matchzero,matchone,matchtwo,matchthree,matchfour,winjackpot) 
dollarswon<-c(matchzero*0,matchone*0,matchtwo*1,matchthree*10,matchfour*100,winjackpot*100000)
dframe <- data.frame(wins,dollarswon)
dframe

# Plot histogram to illustrate distribution of matches
hist(wincount, labels=TRUE, freq=FALSE, xlab="Number of Matches", main="Distribution of Matching Numbers", col="lightgreen", breaks=c(0,1,2,3,4,5), ylim=c(0, 1.0), prob=TRUE)
curve(dnorm(x, mean=mean(wincount), sd=sd(wincount)), add=TRUE, col="darkblue", lwd=2) 

# You lose money. The End.           