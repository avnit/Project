#Parameters
initDate = "2001-01-01"

# In sample
from="2012-01-01"
to = "2013-01-01"

#decisions
BuyChange<-0.1
sellChange<--0.2
buyRSi<-50
sellRsi<-20
BuyCci<-60
SellCci<-20
buyBbanbs<-0.7
sellBbands<-0.3
thresholdVol <- 0
initEq = 50000

# initialize the portfolio and download data 
source('~/Project/R/initialize.R')
# initial the functions that are required for quant start 
source('~/Project/R/functions.R')
# call quant start and get all the data into env
source('~/Project/R/ProjectStart.R')
# call Monte Carlo simulator
source('~/Project/R/MonteCarlo.R')

