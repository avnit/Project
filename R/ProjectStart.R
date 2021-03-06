################################################################################
# script Name : ProjectStart.R
# Purpose : Creates Rule , indicators and trade stats
#
#
################################################################################

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
    strategy = stratBBands, name = "SMA", arguments = list(x = quote(Hi(mktdata)), n = 1),label = "buy"
  )
stratBBands <-
  add.indicator(
    strategy = stratBBands, name = "SMA", arguments = list(x = quote(Hi(mktdata)), n = 1),label = "sell"
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
