# Title: retrieveAnalyzeAlert.r
# Objective: gather data, analyze, and alert to points of interest
# Created by: NR
# Additional editors:

########################################################################
# Preamble
########################################################################

# Set options
options(stringsAsFactors=F)

# Script parameters
block <- 250 # target for each investment size while cash to invest is 'small'

# libraries
library(lattice)
library(quantmod)
library(optparse)

# Structure/document how arguments are to be passed to this script
option_list = list(
  make_option(c("-w", "--working_directory"), type="character", default=NULL,
              help="Path to the base level of the local repository instance", metavar="character"),
  make_option(c("-s", "--sim_directory"), type="character", default=NULL,
              help="Path to the simulation directory", metavar="character"),
  make_option(c("-p", "--position_file"), type="character", default=NULL,
              help="Path to the simulation directory", metavar="character"),
  make_option(c("--sell_threshold"), type="numeric", default=1,
              help="Path to the simulation directory", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

working.dir <- opt$working_directory
sim.dir <- file.path(working.dir,opt$sim_directory)
tmp.positions <- file.path(sim.dir,opt$position_file)
tmp.sThreshold <- opt$sell_threshold
setwd(working.dir)
getwd()

# Source common Fns
source('tickerFns.r')
source("tactFns.r")

# Identify known stocks of interest
pre.tickers <- read.csv(file="activeTickers.csv",header=T) # c("AMZN","GOOG", "MYGN")
tickers <- unique(pre.tickers$ticker)
end <- Sys.time()
begin <- Sys.time()-60*60*24*21

# Retrieve information on known stocks of interest:

# Note, should you desire, you can set different defaults for where ticker info is pulled from:
# setSymbolLookup(QQQ='yahoo',SPY='google') # If run, getSymbols will get QQQ from yahoo and SPY from google

# For when you want to manually pull from a given source w/o using defaults, use the src arg:
# Current ‘src’ methods available are: yahoo, google, MySQL, FRED, csv, RData, oanda, and av.


tmp.summ <- benchmark(tickers=tickers, from=begin,to=end)

# Import data on positions
simPos <- read.csv(file=tmp.positions,header=T)

# Pull current quotes

tmp.id <- identify(tickers=tickers, position=simPos, baseline=tmp.summ$baseline, tactAcqFn=tactAcq01, tactTurnFn=tactTurn01, sThreshold=tmp.sThreshold)


tmp.acq <- proposeAcq(position=simPos, eval=tmp.id$flagged.acq, block.size=250)

# Look for price decreases on low volume days.

tmp.id2 <- identify(tickers=unique(na.omit(tmp.acq$position)), position=tmp.acq, baseline=tmp.summ$baseline, tactAcqFn=tactAcq01, tactTurnFn=tactTurn01, sThreshold=tmp.sThreshold)

tmp.turn <- proposeTurn(position=tmp.acq, flagged=tmp.id2$flagged.turn)

# Repeat the buy operation, then write simPos to file

tmp.id3 <- identify(tickers=unique(na.omit(tmp.turn$position)), position=tmp.turn, baseline=tmp.summ$baseline, tactAcqFn=tactAcq01, tactTurnFn=tactTurn01, sThreshold=tmp.sThreshold)

tmp.acq2 <- proposeAcq(position=tmp.turn, eval=tmp.id3$flagged.acq, block.size=250)

write.csv(x=tmp.acq2, file=tmp.positions,row.names=F)

# Consider backing up simPos occasionally

# Make a fn to take obj's from proposeTurn and proposeAcq and write them to
# simPos

# In the next iteration, have benchmark fn exclude large change days, but collect stats on how often they occur

# Also in next iteration, consider training models for each stock and
# outputing parameters to sep file for trending

# Once we're to this point, run this script regularly so that we can start
# to evaluate simPos as it changes

# Write a separate script to gather stats based on all simPos files to evaluate strategies

# Define low volume as
# Look at decreases > 1 std dev and less than 5% or less than 2 std deviations
# Look at scatter of hi v lows and superpose lineplot of last 5 days
# Avoid holding anything on special event days such as end of quarter

# smod <- lm(Close ~ -1 + ticker + Open, data=summ)
# Plot candidate for purchase against scatter for that ticker, lmline, for
# for all data from yesterday to beginning of window.
# Plot line chart of yesterday to a week previous
# plot todays open on open axis to current quote on close axis


# consider restricting trades for single day movement only
# consider restricting trades for multi-day trends only


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

# For positions sold on a stop loss, watch the slope of the position over time to determine when motion is flat-ish for re-making position.
