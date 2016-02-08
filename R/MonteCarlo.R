library("MASS")

# get data from the env 
data<-mget(my.df.names)
# compute price matrix 
pM<-lapply(cbind,data[[]][,4)

#compute returns matrix
rM <-  apply(pM,2,function(x) diff(log(na.omit(x))))

# why are we getting NAN exceptions 

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

# create Graph in the Plot area 
noofrow <- if(lenght(sPL) %% 2 == 0 ) length(sPL) else length(sPL)
par (mfrow = c(noofrow , 2))
#plot simulated prices
for ( i in 1:length(sPL))
{
plot(sPL[[i]],main="output" + 1,type="l")
}
