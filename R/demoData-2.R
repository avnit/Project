
#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
require(TTR)
require(quantmod)
require(PerformanceAnalytics)
require(quantstrat)

#suppress warings
options("getSymbols.warning4.0" = FALSE)

#Do some house cleaning
#rm(list = ls(.blotter),envir = .blotter)

#set Currency and timezone
currency('USD')
Sys.setenv(TZ = "UTC")


# we need a config file here to get the dates and symbols from the file
# Start Date | End Date | Name of the file
# But in the book this is not done
# for now i am just plugging in values

# MyData <- read.csv(file = "holdings-xlk.csv", header = TRUE, sep = ",")
symbols <- c("HSY","MDLZ",
"RMCF",
"TR" )

initDate = "2001-01-01"
from="2013-01-01"

to = "2016-01-01"

# download all the data
if ( !"AAPL" %in% ls())
{
  #if data is not in the enviornment then get it from Yahoo getSymbol
  suppressMessages(
    getSymbols(
      symbols,from = from, to = to,src = "google"
    )
  )
}

stock(symbols,currency = "USD",multiplier = 1)

# get unemployment number from the file
# again here should be in config file
#

#unEmploymentData <- read.csv("C:/Users/ptemprom/Desktop/SeriesReportUnemployment.csv",header = TRUE,sep = ",")

# unEmploymentData[9,2] Jan 2013
# need to change names of the columns and rows in dataFrame.


#ChangeInUnEmployment<-diff(log(t(unEmploymentData)[seq(1,13),seq(1,11)]))




