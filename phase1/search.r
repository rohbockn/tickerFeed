# Title: search.r
# Objective: search for new companies to read about each day
# Created by: NR
# Additional editors:

########################################################################
# Preamble
########################################################################

# Set options
options(stringsAsFactors=F)

# Load libraries
library(TTR) # Note: https://stackoverflow.com/a/4219418/8469286 indicates that CRAN may not have the maintained version of this library

# Structure/document how arguments are to be passed to this script
option_list = list(
  make_option(c("-w", "--working_directory"), type="character", default=NULL,
              help="Path to the base level of the local repository instance", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
print(str(opt))

# Recommended command to run this script:
# Rscript --vanilla --verbose ~/personal/poc/search.r -w ~/personal/poc > ~/personal/poc/search.out 2>&1
# For help on the script type:
# Rscript --vanilla --verbose ~/personal/poc/search.r --help

# Set working directory:
working.dir <- opt$working_directory
setwd(working.dir)
getwd()

# Confirm internet connection
if (!has_internet()) stop("There is no internet connection")
# if (nslookup(host="www.yahoo.com", ipv4_only=FALSE, multiple=FALSE, error=TRUE)) # not a boolean

# Stocks of interest
active.tickers <- read.csv(file="myTickers.csv",header=T)
active <- unique(pre.tickers$ticker)

# Stocks not currently of interest
inactive.tickers <- read.csv(file="inactiveTickers.csv",header=T)
inactive <- unique(inactive.tickers$ticker)

# Grab available tickers
pool <- stockSymbols()

# Exclude known:
tmp <- pool[-which(pool$Symbol %in% c(active,inactive)),]

# Random sample of unknown
tmp.smp <- sample(x=tmp$Symbol, size=3, replace=F, prob=NULL)

# Report for quick and dirty intro
