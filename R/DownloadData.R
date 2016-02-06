require(TTR)
require(quantmod)
require(PerformanceAnalytics)
require(quantstrat)


options("getsymbols.warning4.0" = FALSE)

initDate = "2001-01-01"
from="2013-01-01"

to = "2016-01-01"

rm(list = ls(.blotter), envir = .blotter)


currency('USD')

Sys.setenv(TZ = "UTC")

stock.str <- c("AAPL","TWTR")

if (!"TWTR" %in% ls())
{
  #if data is not in the enviornment then get it from Yahoo getSymbol
  suppressMessages(
    getSymbols(
      stock.str,from = from, to = to, index.class = c('POSIXt','POSIXct'),env =
        globalenv(),src = "yahoo"
    )
  )

}
tAAPL<-AAPL[,6]
tTWTR<-TWTR[,6]

NewPriceDF <- tAAPL["2014::2016"] - tTWTR ["2014::2015"]

NewStock <-as.xts(AAPL["2014::2016"])

NewStock[,6] <- NewPriceDF[,1]

stock.str <- c("NewStock")

#pdtAAPL <- diff(tAAPL)[-1]
#pdtTWTR <- diff(tTWTR)[-1]

#build the model
#model  <- lm(pdtAAPL ~ pdtTWTR - 1)

#extract the hedge ratio
#hr     <- as.numeric(model$coefficients[1])



SMA2 <- function(x,n = 10,...)
{
  # print(x[,6])
  y <- TTR::SMA(x,n = n,...)

  #  print(y)
  sout <- x[,6]<y[,1]
  out<-as.xts(sout[which(!is.na(sout))])

  #print(head(out))

  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "sma"
  return(out)
}


"osDollarATR" <-
  function(orderside,tradeSize,pctATR,maxPctATR = pctATR, data, timestamp,symbol,prefer = "Open", portfolio,intergerQty = TRUE,atrMod ="nFast",rebal=FALSE,...)
  {

    if (tradeSize > 0 & orderside == "short")
    {
      tradeSize <- tradeSize * -1
    }
    pos <- getPosQty(portfolio,symbol,timestamp)
    atrString <- paste0("sma.", atrMod)
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














initEq = 50000

suppressWarnings(rm("order_book.bbands",pos = .strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos = .blotter))
suppressWarnings(rm(
  "account.st","portfolio.st","stratBBands",'start_t','end_t'
))

currency('USD')
stock(stock.str,currency = 'USD',multiplier = 1)



portfolio.st = 'bbands'
account.st = 'bbands'

initPortf(
  portfolio.st,symbols = stock.str, initDate = initDate ,initPosQty = 0 , currency = "USD"
)
initAcct(
  account.st,portfolios = 'bbands', initDate = initDate,initEq = initEq
)
initOrders(portfolio = portfolio.st,initDate = initDate)

stratBBands <- strategy("bbands")


#first indicator
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "SMA2", arguments = list(x = mktdata, n = 10),label = "nFast"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "SMA2", arguments = list(x = quote(mktdata), n = 30),label = "nSlow"
  )


#add signals:
stratBBands <-
  add.signal(
    stratBBands,name = "sigCrossover",arguments = list(
      columns = c("nFast","nSlow"),relationship = "gt"
    ),label = "long"
  )
stratBBands <-
  add.signal(
    stratBBands,name = "sigCrossover",arguments = list(
      columns = c("nFast","nSlow"),relationship = "lt"
    ),label = "short"
  )
tradeSize <- 10000

#  entry rules
stratBBands <-
  add.rule(
    stratBBands,name = 'ruleSignal', arguments = list(
      sigcol = "long",sigval = TRUE, ordertype = "market",orderside = "long", prefer =
        'Open',osFUN = osDollarATR, tradeSize = tradeSize , pctATR = 2, atrMod = "nFast",replace = FALSE
    ),type = 'enter',label = 'EnterLong',path.dep = TRUE
  )
stratBBands <-
  add.rule(
    stratBBands,name = 'ruleSignal', arguments = list(
      sigcol = "short",sigval = TRUE, ordertype = "market", orderside = "short",prefer =
        'Open',osFUN = osDollarATR, tradeSize = -tradeSize, pctATR = 2, atrMod = "nSlow",replace = FALSE
    ),type = 'enter',label = 'EnterShort',path.dep = TRUE
  )

# Exit rules
stratBBands <-
  add.rule(
    stratBBands,name = 'ruleSignal', arguments = list(
      sigcol = "long",sigval = TRUE, orderqty = 'all', ordertype =
        "market", orderside = "long", replace = FALSE, prefer = "open"
    ),type = 'exit',path.dep = TRUE
  )
stratBBands <-
  add.rule(
    stratBBands,name = 'ruleSignal', arguments = list(
      sigcol = "short",sigval = TRUE, orderqty = 'all', ordertype =
        'market', orderside = NULL, threshold = NULL
    ),type = 'exit',path.dep = TRUE
  )


start_t <- Sys.time()
applyStrategy(strategy = stratBBands , portfolios = 'bbands',debug = TRUE)


getOrderBook('bbands')
end_t <- Sys.time()
print(end_t - start_t)

chart.Posn(Portfolio = 'bbands',Symbol = stock.str)

# Evaluating performance pg 158 book
updatePortf(Portfolio = 'bbands')
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
tstats <-
  tradeStats(Portfolios = portfolio.st,use = "trades",inclZeroDays = FALSE)
print(data.frame(t(tstats)))
