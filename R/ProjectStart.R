source('~/Project/R/initialize.R')
source('~/Project/R/functions.R')



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
    strategy = stratBBands, name = "CCINew", arguments = list(x = quote(Cl(mktdata)), n = 10),label = "nCCI"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "CCINew", arguments = list(x = quote(Cl(mktdata)), n = 30),label = "lCCI"
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
