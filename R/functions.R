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







