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


source('~/Project/R/initialize.R')
source('~/Project/R/functions.R')



initEq = 50000

suppressWarnings(rm("order_book.bbands",pos = .strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos = .blotter))
suppressWarnings(rm(
  "account.st","portfolio.st","stratBBands",'start_t','end_t'
))






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
    strategy = stratBBands, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 1),label = "buy"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 1),label = "sell"
  )



#add signals:
stratBBands <-
  add.signal(
    stratBBands,name="sigThreshold", arguments = list(column = "SMA.buy",threshold=thresholdVol, relationship = "gt",cross=FALSE),label = "long"
  )
stratBBands <-
  add.signal(
    stratBBands,name="sigThreshold", arguments = list(column = "SMA.sell",threshold=thresholdVol, relationship = "lt",cross=TRUE
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

