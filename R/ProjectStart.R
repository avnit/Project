source('~/Project/R/demoData-2.R')
# Computes CCI
CCINew <- function(x,n = 10,...)
{
  # print(x[,6])
  y <- TTR::CCI(x,n = n,...)
  out<-y$CCI
  #  print(y)


  #print(head(out))

  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "cci"
  return(out)
}


# Computes RSI
RSNNew <- function(x,n = 10,...)
{
  # print(x[,6])
  y <- TTR::RSN(x,n = n,...)
  out<-y$RSN
  #  print(y)


  #print(head(out))

  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "RSN"
  return(out)
}

# bbands n=14 n=30
BBandsNew <- function(x,n = 10,...)
{
  # print(x[,6])
  y <- TTR::BBands(x,n = n,...)
  out<-y$BBands
  #  print(y)


  #print(head(out))

  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "bbands"
  return(out)
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
    atrString <- paste0("cci.", atrMod)
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
    strategy = stratBBands, name = "CCINew", arguments = list(x = mktdata, n = 10),label = "nFast"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "CCINew", arguments = list(x = quote(mktdata), n = 30),label = "nSlow"
  )



stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "RSINew", arguments = list(x = mktdata, n = 10),label = "nFast"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "RSINew", arguments = list(x = quote(mktdata), n = 30),label = "nSlow"
  )


stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "BBandsNew", arguments = list(x = mktdata, n = 10),label = "nFast"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "BBandsNew", arguments = list(x = quote(mktdata), n = 30),label = "nSlow"
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
