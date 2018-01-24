# Title: accounting.r
# Objective: Track dispositions of deposits, apportion fund worth to designations including tax
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
              help="Path to the base level of the local repository instance", metavar="character")
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
