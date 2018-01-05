# Title: collect.r
# Objective: collect data every 15 minutes to look at volatility
# Created by: NR
# Additional editors:

########################################################################
# Preamble
########################################################################

# Print a timestamp for the log:
cat("Timestamp: ",Sys.time())

# Set options
options(stringsAsFactors=F)

library(quantmod)
library(curl)
library(optparse)

# Structure/document how arguments are to be passed to this script
option_list = list(
  make_option(c("-w", "--working_directory"), type="character", default=NULL,
              help="Path to the base level of the local repository instance", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
print(str(opt))

# Recommended command to run this script:
# Rscript --vanilla --verbose ~/personal/poc/collect.r -w ~/personal/poc > ~/personal/poc/collect.out 2>&1
# For help on the script type:
# Rscript --vanilla --verbose ~/personal/poc/collect.r --help

# Set working directory:
working.dir <- opt$working_directory
setwd(working.dir)
getwd()

# Confirm internet connection
if (!has_internet()) stop("There is no internet connection")
# if (nslookup(host="www.yahoo.com", ipv4_only=FALSE, multiple=FALSE, error=TRUE)) # not a boolean

# Stocks of interest
pre.tickers <- read.csv(file="activeTickers.csv",header=T)
tickers <- unique(pre.tickers$ticker)

# Collect data:
all.tickers <- paste(tickers,collapse=";")
# getQuote(all.tickers)
tmp <-
getQuote(all.tickers,
  what=yahooQF(c("Open", "Trade Time", "Last Trade (Price Only)"))
)
tmp$ticker <- rownames(tmp)

write.table(x=tmp, file="collect.csv",row.names=F, col.names=F, append=TRUE, sep=",")

# yahooQF

# Set up a chron job to run every 15 minutes on a reg day
# */15 7-14 * * 1-5 ~/personal/poc/collect.sh
# Have R check for an internet connection and have it check for an open market
