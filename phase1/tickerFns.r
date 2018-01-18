# Title: tickerFns.r
# Objective: central script with most common ticker functions
# Created by: NR
# Additional editors:

# Those originally written for retrieveAnalyzeAlert.r


benchmark <- function(tickers, from, to){
  getSymbols(tickers, from = "2017-10-01", to = end, src='yahoo')
  # getSymbols(tickers, from = "2017-10-01", to = end, src='google', return.class="data.frame")

  # Consolodate ticker data to facilitate easier analysis:
  summ <- NULL
  for (i in tickers) {
    tmp <- get(i)
    tmp <- as.data.frame(tmp)
    names(tmp) <- gsub(names(tmp),pattern="^.+\\.(.*)",replacement="\\1")
    tmp$ticker <- i
    tmp$date <- as.POSIXct(rownames(tmp))
    summ <- rbind(summ,tmp)
    rm(tmp)
  }

  # Get historical baselines:
  # smfn <- function(dat, response, aggvar, smfn) {
  #   tmp01 <- with(dat,aggregate(x=list()))
  # }

  hdat <- read.csv(file='collect.csv',header=T)
  hdat$Trade.Time <- as.POSIXct(hdat$Trade.Time)
  hdat$day <- as.POSIXct(strptime(hdat$Trade.Time, format="%F"))
  hsumm01 <- with(hdat,aggregate(x=list(sd.daily=Last),by=list(day=day,ticker=ticker),FUN=function(x) sd(x,na.rm=T)))
  hsumm11 <- with(hsumm01,aggregate(x=list(med.daily.sd=sd.daily),by=list(ticker=ticker),FUN=median))
  # hsumm12 <- with(hsumm01,aggregate(x=list(q25.daily.sd=sd.daily),by=list(ticker=ticker),FUN=function(x) quantile(x,probs=.25)))
  # hsumm13 <- with(hsumm01,aggregate(x=list(q75.daily.sd=sd.daily),by=list(ticker=ticker),FUN=function(x) quantile(x,probs=.75)))

  hsumm02 <- with(hdat,aggregate(x=list(mn.daily=Last),by=list(day=day,ticker=ticker),FUN=function(x) mean(x,na.rm=T)))
  hsumm21 <- with(hsumm02, aggregate(x=list(med.mn.daily=mn.daily),by=list(ticker=ticker),FUN=median))
  # hsumm22 <- with(hsumm02,aggregate(x=list(q25.daily.mn=mn.daily),by=list(ticker=ticker),FUN=function(x) quantile(x,probs=.25)))
  # hsumm23 <- with(hsumm02,aggregate(x=list(q75.daily.mn=mn.daily),by=list(ticker=ticker),FUN=function(x) quantile(x,probs=.75)))

  hsumm <- merge(hsumm11,hsumm21,by='ticker',all=T)
  hsumm$cvhat <- with(hsumm,med.daily.sd/med.mn.daily)
  # hsumm <- merge(hsumm,hsumm12,by='ticker',all=T)
  # hsumm <- merge(hsumm,hsumm13,by='ticker',all=T)
  # hsumm <- merge(hsumm,hsumm22,by='ticker',all=T)
  # hsumm <- merge(hsumm,hsumm23,by='ticker',all=T)
  # hsumm$skewRatio <- with(hsumm, (q75.daily.mn-med.mn.daily)/(med.mn.daily-q25.daily.mn)) # do not use this, calculate each day, then take the median!
  return(list(baseline=hsumm))
  }

  identify <- function(tickers, baseline, position, tactAcqFn, tactTurnFn){
    eval.obj <-
      getQuote(tickers,
        what=yahooQF(c("Open", "Trade Time", "Last Trade (Price Only)","Volume"))
      )
    eval.obj$ticker <- rownames(eval.obj)
    # browser()
    eval.acq <- merge(eval.obj,baseline,by='ticker',all=T)
    eval.turn <- merge(position, eval.obj, by.x='position', by.y='ticker', all.x=T)
    eval.turn <- merge(eval.turn,baseline, by.x='position',by.y='ticker',all.x=T)

    # Identify candidates for purchase
    evaluated.acq <- tactAcqFn(eval.acq)
    evaluated.turn <- tactTurnFn(eval.turn)
    return(list(flagged.acq=evaluated.acq, flagged.turn=evaluated.turn))
  }

  proposeAcq <- function(position, eval, block.size, repeat=F) {
    liquid <- with(position,sum(cash.transaction))

    # Calculate target purchase size should a given ticker be chosen
    eval$pcnt <- with(eval,ifelse(flag.buy,yes=floor(block.size/Last),no=0))
    # Simulate a 'buy'
    for(i in eval$ticker) { # Please add a control to prevent second purchase in existing stake!
      tmp <- eval[which(eval$ticker==i),]
      if(tmp$flag) {
        tmp.timestamp <- Sys.time()
        tmp.id <- max(position$id)+1
        proposed <- data.frame(timestamp=tmp.timestamp, id=tmp.id, position=tmp$ticker, count=tmp$pcnt, basis.id=paste(strftime(tmp.timestamp,format="%F"),tmp.id,sep="_"), type='acquire', price=tmp$Last, cash.transaction=with(tmp,-pcnt*Last))
        if(proposed$cash.transaction+liquid<0) break()
        if(!repeat & (proposed$position %in% position$position)) break()
        position <- rbind(position,proposed)
      }
      liquid <- with(position,sum(cash.transaction))
    }

    return(position)
  }

  proposeTurn <- function(position, flagged, block.size){
    for(i in flagged$ticker) { # Please add a control to prevent second purchase in existing stake!
      tmp <- flagged[which(flagged$ticker==i),]
      if(tmp$flag_sell) {
        tmp.timestamp <- Sys.time()
        tmp.id <- max(position$id)+1
        tmp.basis.id <- tmp$basis.id
        proposed <- data.frame(timestamp=tmp.timestamp, id=tmp.id, position=tmp$ticker, count=tmp$pcnt, basis.id=tmp.basis.id,  type='sell', price=tmp$Last, cash.transaction=with(tmp,count*Last))
        position <- rbind(position,proposed)
      }
    }
    return(position)
  }
