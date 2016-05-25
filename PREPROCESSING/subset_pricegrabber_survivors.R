#!/usr/bin/Rscript --slave

# use -h or --help to see usage and options

suppressPackageStartupMessages(library("argparse"))

parser <- ArgumentParser()
parser$add_argument("logFile", nargs=1, help="pricegrabber logfile to be read")
parser$add_argument("mergedFile", nargs=1, help="merged events file to be read")
parser$add_argument("setDirectory", nargs=1, help="directory to which sets are saved")
parser$add_argument("reportFile", nargs=1, help="set report file to be returned")
args <- parser$parse_args()

log <- read.csv(args$logFile)

filtered <- read.csv(args$mergedFile)

# -----------------------------------------------------------
# 252 (= 1-year: roughly 4 events prior) + 20 day margin
# in case of varying event date of ev-4,
window1 <- c(-((252*1)+20), -6)
# 1 week before + 1 week after event
window2 <- c(-5, 5)
# -----------------------------------------------------------

# calculate if there are enough datapoints to support using the event
log$haveRange <- (log$position >= -(window1[[1]]-1)+2) &
				 (log$datapoints-log$position >= window2[[2]])

# merge to find events for which we could find prices
filtered$UID = paste(filtered$symbol, filtered$tradingDate, sep="_")
log$UID = paste(log$symbol, log$tradingDate, sep="_")
events <- merge(filtered,
				subset(data.frame(UID=log$UID, haveRange=log$haveRange),
					   subset=(haveRange == TRUE)),
				by="UID")

# forming multi-year subsets into groups ================

# the use of loops in R is acceptable here as we are not transforming data,
# just forming groups

# useful for obtaining a random subsample of a df rows, generic
sample.df <- function(df, n){
	df[sample(nrow(df), n), , drop = FALSE]
	}

# if DATA/SUBSETS does not exist, create it
ifelse(!dir.exists(args$setDirectory),
	    dir.create(args$setDirectory), FALSE)

group <- list()
i <- 1
for (mySign in c(1, -1)){
	for (myCiso in c("CHN", "USA")){
		year="2015"
		# We shouldn't need to filter at this point
		temp <- subset(events, subset=(sign == mySign & ciso == myCiso))

		# eliminate clustering
		# get list of unique dates of the subset
		uniqueTradingDates <- as.list(unique(temp$tradingDate))
		temp <- do.call(rbind, lapply(uniqueTradingDates, FUN=function(d){
			# randomly sample 1 element from a subset
			# where trading dates match each unique date
			sample.df(subset(temp, subset=(tradingDate == d)), 1)
			}))

		# form UID for the subset
		groupUID <- paste("A_{", myCiso, ",", mySign, "}", sep="")

		nEvents <- nrow(temp)
		if (is.null(nEvents)){
			nEvents <- 0
		}

		group[[i]] <- list(groupUID=groupUID, sign=mySign, ciso=myCiso,
						   year=year, nEvents=nEvents, events=temp)
		write.csv(group[[i]]$events,
				  file=file.path(args$setDirectory, paste(group[[i]]$groupUID, ".csv", sep="")),
				  row.names=FALSE)
		i <- i+1
		}
	}

# ============= set report

report <- NULL
for (i in 1:length(group)){
	ret <- data.frame(sign=group[[i]]$sign,
					  ciso=group[[i]]$ciso,
					  year=group[[i]]$year,
					  nEvents=group[[i]]$nEvents)
	report <- rbind(report, ret)
	}

write.csv(report, file=args$reportFile, row.names=FALSE)

# # joining the subsets together
all <- rbind(group[[1]]$events, group[[2]]$events, group[[3]]$events, group[[4]]$events)

# save this for presentation later
# TODO: replace paste with file.path



write.csv(all, file.path(args$setDirectory, "A.csv"), row.names=FALSE)


