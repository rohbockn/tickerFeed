# Title: daySummary.r
# Objective: Gather data on positions and report at the end of day
# Created by: NR
# Additional editors:

########################################################################
# Preamble
########################################################################

# Set options
options(stringsAsFactors=F)

# libraries
library(lattice)
library(quantmod)
library(optparse)
library(reshape2)

# Structure/document how arguments are to be passed to this script
option_list = list(
  make_option(c("-w", "--working_directory"), type="character", default=NULL,
              help="Path to the base level of the local repository instance", metavar="character"),
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

working.dir <- opt$working_directory
setwd(working.dir)
getwd()

# Source common Fns
source('tickerFns.r')
source("tactFns.r")

########################################################################
# Gather data:
########################################################################

history <- read.csv(file=file.path(working.dir,'sim001','simPos.csv'),header=T)
history$timestamp <- with(history,as.POSIXct(timestamp))
# Determine if positions are closed
history.wd01 <- dcast(formula=basis.id + position ~ type, value.var='count', fun.aggregate=sum, data=history)
history.wd02 <- dcast(formula=basis.id + position ~ type, value.var='cash.transaction', fun.aggregate=sum, data=history)

# start here tomorrow, finish this and use as duration of hold
history.wd03 <- dcast(formula=basis.id + position ~ type, value.var='timestamp',fun.aggregate=function(x) max(x)-min(x),data=history)

history.wd01 <- history.wd01[which(!is.na(history.wd01$basis.id)),]
history.wd02 <- history.wd02[which(!is.na(history.wd02$basis.id)),]

history.wd01$closed <- with(history.wd01, ifelse(acquire==sell,1,0))

history.wd03 <- merge(history.wd01,history.wd02,by=c('basis.id','position'),all=T, suffixes=c(".count",".total_price"))

history.wd <- history.wd03[,-grep(names(history.wd03),pattern="^Deposit.*")]
history.wd$date <- as.POSIXct(gsub(x=history.wd$basis.id,pattern="^(.*)_\\d",replacement="\\1"))
history.wd$delta <- with(history.wd, acquire.total_price+sell.total_price)
history.wd$success <- with(history.wd, ifelse(delta>0,1,0))
history.wd$percent.return <- with(history.wd,delta/abs(acquire.total_price)*closed)
full.outlay <- with(history.wd,aggregate(x=list(day.outlay=abs(acquire.total_price)),by=list(date=date),FUN=sum))
history.wd <- merge(history.wd,full.outlay,by='date',all=T)
day.profit <- with(history.wd,aggregate(x=list(day.profit=delta*closed), by=list(date=date),FUN=sum))
history.wd <- merge(history.wd,day.profit,by='date',all=T)
day.utilization <- with(history.wd,aggregate(x=list(day.turned=abs(acquire.total_price)*closed),by=list(date=date),FUN=sum))
history.wd <- merge(history.wd, day.utilization,by='date',all=T)
history.wd$day.utilization <- with(history.wd, day.turned/day.outlay)
history.wd$day.return <- with(history.wd, day.profit/day.outlay)
history.wd$day.turned.return <- with(history.wd, day.profit/day.turned)


# Next steps include
# accountability. Script running every day to take note of deposits and attribute percentages of fund to deposit source/designation
# Maybe look at position reports and rebalance designation percentages at the end of the day from deposits and current levels.  Use bases instead of current value for non-closed positions as no profit has yet been realized.
# Install latex or markdown language to output a 'whitepaper' report at the end of each day
# When doing the day summary, use the major indices (dia, nasdaq, s&p500, etc) as baselines to judge performance by.
# Make sure reports will work for prod as well as sim.

# Have daily report identify candidate positions for start of next day

# Consider having any profits rebalance every day proportionate to all designations.  Consider having a designation for tax, tithes, etc.
