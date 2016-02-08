#getSymbols()

#plot the prices of these stocks
par(mfrow = c(3,2))
plot(AAPL[,6], main = "AAPL")
plot(QQQQ[,6], main = "QQQQ")
plot(SPY[,6], main = "SPY")
plot(GOOG[,6], main = "GOOG")
plot(CVX[,6], main = "CVX")
par(mfrow = c(1,1))

#compute price matrix
pM <- cbind(AAPL[,6], QQQQ[,6], SPY[,6], GOOG[,6], CVX[,6])

#compute returns matrix
rM <-  apply(pM,2,function(x) diff(log(x)))

#look at pairwise charts
pairs(coredata(rM))

#compute the covariance matrix
covR <- cov(rM)

#use this covariance matrix to simulate normal random numbers
#that share a similar correlation structure with the actual data
meanV <- apply(rM, 2, mean)
rV    <- mvrnorm(n = nrow(rM), mu = meanV, Sigma = covR)

#simulate prices based on these correlated random variables

#calculate mean price
p0 <- apply(pM,2,mean)
sPL <- list()
for(i in 1:ncol(rM)){
  sPL[[i]] <-round(p0[i]*exp(cumsum(rV[,i])),2)
}

#plot simulated prices
par(mfrow = c(3,2))
plot(sPL[[1]],main="AAPLsim",type="l")
plot(sPL[[2]], main = "QQQQ sim",type = "l")
plot(sPL[[3]], main = "SPY sim", type = "l")
plot(sPL[[4]], main = "GOOG sim",type = "l")
plot(sPL[[5]], main = "CVX sim", type = "l")
