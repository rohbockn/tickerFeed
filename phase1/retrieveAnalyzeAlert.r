# Title: retrieveAnalyzeAlert.r
# Objective: gather data, analyze, and alert to points of interest
# Created by: NR
# Additional editors:

########################################################################
# Preamble
########################################################################

# Set options
options(stringsAsFactors=F)

# libraries
library(lattice)

libraries <- c("quantmod") # "dygraphs",
lapply(libraries, function(x) if (!(x %in% installed.packages())) { install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Identify known stocks of interest
pre.tickers <- read.csv(file="activeTickers.csv",header=T) # c("AMZN","GOOG", "MYGN")
tickers <- unique(pre.tickers$ticker)
end <- Sys.Date()

# Retrieve information on known stocks of interest:

# Note, should you desire, you can set different defaults for where ticker info is pulled from:
# setSymbolLookup(QQQ='yahoo',SPY='google') # If run, getSymbols will get QQQ from yahoo and SPY from google

# For when you want to manually pull from a given source w/o using defaults, use the src arg:
# Current ‘src’ methods available are: yahoo, google, MySQL, FRED, csv, RData, oanda, and av.
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

# Import data on positions
simPos <- read.csv(file=file.path('sim001',"simPos.csv"),header=T)
liquid <- with(simPos[which(simPos$position=='cash'),],count)

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

# Pull current quotes
eval <-
getQuote(tickers,
  what=yahooQF(c("Open", "Trade Time", "Last Trade (Price Only)","Volume"))
)
eval$ticker <- rownames(eval)
eval <- merge(eval,hsumm,by='ticker',all=T)

# Identify best candidates for purchase
# Look for price decreases on low volume days.
# Define low volume as
# Look at decreases > 1 std dev and less than 5% or less than 2 std deviations
# Look at scatter of hi v lows and superpose lineplot of last 5 days
# Avoid holding anything on special event days such as end of quarter

smod <- lm(Close ~ -1 + ticker + Open, data=summ)
# Plot candidate for purchase against scatter for that ticker, lmline, for
# for all data from yesterday to beginning of window.
# Plot line chart of yesterday to a week previous
# plot todays open on open axis to current quote on close axis

# Look for decrease > threshold
eval$threshold <- with(eval,Open-2*med.daily.sd)
eval$flag <- with(eval,Last<threshold)
# consider restricting trades for single day movement only
# consider restricting trades for multi-day trends only

# Simulate a 'buy'
block <- 250
eval$pcnt <- with(eval,ifelse(flag,yes=floor(block/Last),no=0))
eval[with(eval,which(flag==TRUE)),c('Last','ticker','pcnt','Trade Time')]

# Identify candidate for sell
# Look for increase > threshold


# Make trades

# Monitor trade by strategy/threshold and when net gain becomes neg or successes are less than 60%, retire methods



# LOok at distn of prices on low volume days as opposed to high volume days

# Set up simulation to mock trade at several different thresholds.  Pick threshold for 1-3 trades a day or fewer if more profitable
# Project trades out at assumed frequency close to current frequency, a 10 day, 30 day etc to compare
# Simulate lags in trading and grab prices at those times to anticipate effectiveness of targeted trades
# Because processes tend to regresss to a mean, look at a window for mean that you expect pricess to regress to absent special cause
# getQuote() should give a closer to real-time estimate
# Track the proportion of times trades result in profits
# Track the profit/loss of each trade
# Plot stock price timeline and superpose points on buy and sell dates for visual of how close to local minima/maxima you are

# a = seq(1, 51, by =1)
#
# time = index(AMZN)
# time = time[a]
# time = format(time, format = "%d.%m.")
# for (i in 1:3){
#   assign(paste0(tickers[i], ".1"), Cl(get(tickers[i])))
# }
# AMZN.2 = as.numeric(AMZN.1)
# GOOG.2 = as.numeric(GOOG.1)
# MSFT.2 = as.numeric(MSFT.1)
#
#
# abc = as.numeric(match("27.10.", time))

# plot(AMZN.2, type = "l", xlab = "time", ylab = "price (USD, NASDAQ)", xaxs = "i", xaxt = "n", main = "Share price Amazon Inc. (Oct 17 - Dec 17)", sub = "qrtly results announced at oct 26th")
# axis(1,a, labels = time) #a is a numeric vector, time a character vector
# abline(v = abc, col = "red", lty = "dotted") #abc is a number (=20)
# abline(h = 972.43, col = "red", lty = "dotted")
# abline(h = 1100.95, col = "red", lty = "dotted")
# text(abc, "my text here", col = "red", srt = 90) #abc see above​
