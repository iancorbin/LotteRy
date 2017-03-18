# WA Lottery Lotto Game Simulation
# Author: Ian Corbin from Seattle, WA
# Web: http://www.iancorbin.com / Twitter: @iancorbin
# Game Rules: http://www.walottery.com/JackpotGames/Lotto.aspx

# set.seed(9999)  # Set seed if you want to reproduce the results

# Set game variables
cost <- .5        # Cost per play (in dollars)
trials <- 100000  # Number of tickets to purchase
numlow <- 1       # Lowest number available to pick
numhigh <- 49     # Highest number available to pick
numpick <- 6      # Number of balls available to pick


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
c("$0","$0","$0","$3","$30","$1000","$1000000")[apply(m2,1,function(x) sum(x%in%w))+1]

avgWin = mean(wincount)
stdWin = sd(wincount)

win <- sum(c(0,0,0,3,30,1000,1000000)[apply(m2,1,function(x) sum(x%in%w))+1])
bet <- cost * trials
delta <- win - bet
winningpercentage <- (win - bet)/bet
paste("Won $",format(win, big.mark=","),sep="")
paste("Spent $",format(bet, big.mark=","),sep="")
paste("Delta $",format(delta, big.mark=","),sep="")
paste("Winning %",format(winningpercentage, big.mark=","),sep="")

matchzero <- length(which(wincount == 0)) 
matchone <- length(which(wincount == 1)) 
matchtwo <- length(which(wincount == 2))
winnothing <- sum(matchzero + matchone + matchtwo)

matchthree <- length(which(wincount == 3)) 
matchfour <- length(which(wincount == 4)) 
matchfive <- length(which(wincount == 5)) 
winjackpot <- length(which(wincount == 6)) 

winsomething <- length(which(wincount > 2)) 

paste("Won Something: ",format(winsomething, big.mark=","),sep="")

w1 <- paste("$0: ",format(winnothing, big.mark=","),sep="")
w2 <- paste("$3: ",format(matchthree, big.mark=","),sep="")
w3 <- paste("$30: ",format(matchfour, big.mark=","),sep="")
w4 <- paste("$1000: ",format(matchfive, big.mark=","),sep="")
w5 <- paste("$1,000,000: ",format(winjackpot, big.mark=","),sep="")

paste("Possible Combos: ",format(combos, big.mark=",", scientific = FALSE),sep="")
paste("Jackpot Probability: ",format(1/combos, big.mark=",", scientific = FALSE),sep="")
paste("Jackpot Odds: 1:",format(combos, big.mark=",", scientific = FALSE),sep="")

# Print data frame of matches
wins<-c(matchzero,matchone,matchtwo,matchthree,matchfour,matchfive,winjackpot) 
dollarswon<-c(matchzero*0,matchone*0,matchtwo*0,matchthree*3,matchfour*30,matchfive*100,winjackpot*1000000)
dframe <- data.frame(wins,dollarswon)
dframe

# Plot histogram to illustrate distribution of matches
hist(wincount, labels=TRUE, freq=FALSE, xlab="Number of Matches", main="Distribution of Matching Numbers", col="lightgreen", breaks=c(0,1,2,3,4,5,6), ylim=c(0, 1.0))
curve(dnorm(x, mean=mean(wincount), sd=sd(wincount)), add=TRUE, col="darkblue", lwd=2) 

# You lose money. The End. 