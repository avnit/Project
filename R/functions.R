bindicator <- function(x,...)
{
  indicator<-as.data.frame(x)
  # High is our indicator
  indicator$bindicator = Hi(x)

# colnames(indicator) <- "bindicator"
 return(indicator)
}

sindicator <- function(x,...)
{
  indicator<-as.data.frame(x)
   # High is our indicator
  indicator$sindicator = Hi(x)
  return(indicator)
}


# OS dollar ATR function
"osDollarATR" <-
  function(orderside,tradeSize,pctATR,maxPctATR = pctATR, data, timestamp,symbol,prefer = "Open", portfolio,intergerQty = TRUE,atrMod ="nFast",rebal=FALSE,...)
  {

    if (tradeSize > 0 & orderside == "short")
    {
      tradeSize <- tradeSize * -1
    }
    pos <- getPosQty(portfolio,symbol,timestamp)
    atrString <- paste0("SMA.buy")
    atrCol <- grep(atrString,colnames(mktdata))

    if (length(atrCol) == 0) {
      stop(paste("term" , atrString))
    }
    atrTimeStamp <- mktdata[timestamp,atrCol]

    if (is.na(atrTimeStamp) | atrTimeStamp == 0)
    {
      STOP("ERROR")
    }


    dollarATR <- pos * atrTimeStamp
    desiredDollarATR <-pctATR * tradeSize
    remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR

    if (orderside == "long")
    {
      qty <-
        min(tradeSize * pctATR / atrTimeStamp,remainingRiskCapacity / atrTimeStamp)
    }
    else {
      qty <-
        max(tradeSize * pctATR / atrTimeStamp, remainingRiskCapacity / atrTimeStamp)
    }
    if (intergerQty)
    {
      qty <- trunc(qty)
    }

    if (orderside == "long" & qty < 0)
    {
      qty <- 0
    }
    if (orderside == "short" & qty > 0)
    {
      qty <- 0
    }
    return(qty)


  }

setIndicator <- function (stockName,stringName)
{
newDataObject <- as.data.frame(get(stockName))
suppressMessages(
  getSymbols(stockName,from = from, to = to, index.class = c('POSIXt','POSIXct'),env =
               globalenv(),src = "google"
  )
)
DataXTSObject <-as.xts(get(stockName))

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
names<-paste(stringName)
temp$indicator<- 0

temp$indicator[temp$CHG_PCT_1D > BuyChange & (temp$cci.x > BuyCci || temp$EMA.x > buyRSi || temp$pct > buyBbanbs) ]<- 1
temp$indicator[temp$CHG_PCT_1D < sellChange  & (temp$cci.x < SellCci || temp$EMA.x < sellRsi || temp$pct < sellBbands) ]<--1


temp[,8]<-temp$indicator
assign(names,as.xts(temp[,c(seq(5,9))]))
}







