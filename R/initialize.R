################################################################################
# script Name : Initialize.R
# Purpose : Loads intial portfolio and sugar prices from Qunadl
#
# Installs :
# #Required in windows . Hash this in Mac
# install.packages("curl")
# install.packages("devtools")
# install_github('quandl/R-package')
# install.packages("quantstrat", repos="http://R-Forge.R-project.org")
#
################################################################################



require(TTR)
require(quantmod)
require(PerformanceAnalytics)
require(quantstrat)

library(curl)



library(openair)

options("getsymbols.warning4.0" = FALSE)



rm(list = ls(.blotter), envir = .blotter)


currency('USD')

Sys.setenv(TZ = "UTC")

# The stock list is maintained in a CSV file and we need to convert that into a vector.
stockList <- read.csv(file="~/Project/Data/holdings-xlk.csv",header = TRUE,sep=",")
stock.str <-as.vector(t(stockList[1]))

# get sugar price as dataframe

sugarPrice <- as.data.frame(read.csv(file="~/Project/Data/BloombergSugarPrices.csvWithChange",header=TRUE,sep=","))
row.names(sugarPrice)<-as.Date(sugarPrice$Date, "%Y-%m-%d")
sugarPrice$Date2<-as.Date(sugarPrice$Date, "%Y-%m-%d")
sugarPriceInSample <- as.data.frame(as.xts(sugarPrice)[paste0(from,"::", to)])



# Loop only if we need to download the data.

  #if data is not in the enviornment then get it from Yahoo getSymbol
  suppressMessages(
    getSymbols(
      stock.str,from = from, to = to, index.class = c('POSIXt','POSIXct'),env =
        globalenv(),src = "google"
    )
  )


  stock(stock.str,currency = 'USD',multiplier = 1)


list.function <-  function() {

  sapply(stock.str,get,environment(),simplify = FALSE)
}
list.function() -> mylist
my.df.names <- names(mylist)

source('~/Project/R/functions.R')

for (i in 1:length(mylist) )
{

  newDataObject <- as.data.frame(get(my.df.names[i]))
  suppressMessages(
  getSymbols(my.df.names[i],from = from, to = to, index.class = c('POSIXt','POSIXct'),env =
               globalenv(),src = "google"
  )
  )
  DataXTSObject <-as.xts(get(my.df.names[i]))

  ##cci
  cci<-as.data.frame(TTR::CCI(HLC=HLC(DataXTSObject),n=20,maType='EMA',rm.na=TRUE))
  cci$date<-as.Date(row.names(cci),"%Y-%m-%d")



  #RSI
  rsi<-as.data.frame(TTR::RSI(price=Cl(DataXTSObject)))
  rsi$date<-as.Date(row.names(rsi),"%Y-%m-%d")
  #bbands
  bbands<-as.data.frame(TTR::BBands(HLC=HLC(DataXTSObject),n=20,maType='EMA'))
  bbands$date<-as.Date(row.names(bbands),"%Y-%m-%d")

  #price merger
  temp <-merge(newDataObject,sugarPriceInSample,by.x="row.names",by.y = "Date2")
  row.names(temp)<-temp$Row.names
  #CCI merger
  temp<-merge(temp,na.omit(cci),by="row.names")
  row.names(temp)<-temp$Row.names

  temp<-merge(temp,na.omit(rsi),by="row.names")
  row.names(temp)<-temp$Row.names
  temp<-merge(temp,na.omit(bbands),by="row.names")
  row.names(temp)<-temp$Row.names
  names<-paste(my.df.names[i])
  temp$indicator<- 0

    if (temp$CHG_PCT_1D > BuyChange || temp$cci.x > BuyCci || temp$EMA.x > buyRSi || temp$pct > buyBbanbs ])
    {
      temp$indicator<- 1
    }

    if(temp$CHG_PCT_1D < sellChange || temp$cci.x < SellCci || temp$EMA.x < sellRsi || temp$pct < sellBbands ])
    {
      temp$indicator<- -1
    }

  temp[,8]<-temp$indicator
  assign(names,as.xts(temp[,c(seq(5,9))]))



}










