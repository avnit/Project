require(TTR)
require(quantmod)
require(PerformanceAnalytics)
require(quantstrat)

#Required in windows . Hash this in Mac
#install.packages("curl")
library(curl)


# price of sugar
#install.packages("devtools")
library(devtools)
#install_github('quandl/R-package')
library(Quandl)

options("getsymbols.warning4.0" = FALSE)

initDate = "2001-01-01"

# In sample
from="2013-01-01"
to = "2016-01-01"

rm(list = ls(.blotter), envir = .blotter)


currency('USD')

Sys.setenv(TZ = "UTC")

# The stock list is maintained in a CSV file and we need to convert that into a vector.
stock.str <-as.vector(t(read.csv(file="~/Project/Data/holdings-xlk.csv",header = TRUE,sep=",")[1]))

# Loop only if we need to download the data.
if (!"HSY" %in% ls())
{
  #if data is not in the enviornment then get it from Yahoo getSymbol
  suppressMessages(
    getSymbols(
      stock.str,from = from, to = to, index.class = c('POSIXt','POSIXct'),env =
        globalenv(),src = "google"
    )
  )

}

# get sugar price as dataframe
as.data.frame(Quandl("FRED/M04031US35620M267NNBR")) -> SugarPrices




