library("MASS")

#compute price matrix
data<-mget(my.df.names)
pM<-na.omit(data[[1]][,4])
#for(i in seq(2,length(data)))
for(i in seq(2,10))
{
  pM<-cbind(pM,data[[i]][,4])
}


#compute returns matrix
rM <-  apply(pM,2,function(x) diff(log(x)))

# why are we getting NAN exceptions

#look at pairwise charts
#
tryCatch({
  rM[is.na(rM)] <- 0
  par(mar = rep(2, 4))
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
# can use Rcpp function that we created noofrow = ElvisOperator(length(sPL) %% 2 == 0) ,
noofrow <- if(length(sPL) %% 2 == 0 ) length(sPL)/2  else length(sPL)/2 + 1
par (mfrow = c(3 , 2))
#plot simulated prices
for ( i in c(1,2,4,5,6,7))
{
# plot only works in big screen
  plot(sPL[[i]],main=paste0("output",i),type="l")
}
},error=function(e){print(e)})
