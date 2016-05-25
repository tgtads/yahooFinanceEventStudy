#!/usr/bin/Rscript --slave

# script for selecting stocks of interest, while editing some of the values

# use -h or --help to see usage and options

suppressPackageStartupMessages(library("argparse"))

parser <- ArgumentParser()
parser$add_argument("-f", "--filter", action="store_true",
                    help="select only events with desired characteristics", default=FALSE)
parser$add_argument("eventsFile", nargs=1, help="event list to be read")
parser$add_argument("stocksFile", nargs=1, help="stock profile file to be read")
parser$add_argument("changeFile", nargs=1, help="symbol change file to be read")
parser$add_argument("mergedFile", nargs=1, help="csv file to be returned")

args <- parser$parse_args()

# na.locf(fromLast=True) provides nocb()
suppressPackageStartupMessages(library("zoo"))

EPOCHST <- "1970-01-01"

events <- read.csv(args$eventsFile)

# to force use as date
events$date <- as.Date(as.character(events$date))

events$eps <- as.numeric(as.character(events$eps))
events$eeps <- as.numeric(as.character(events$eeps))

events$surprise <- 100*((events$eps/events$eeps)-1)
events$sign <- sign(events$surprise)

# find the trading day on or following the announcement -------
# F-F factor calendar useful for this
tradingCalendar <- as.Date(as.character(read.csv("DATA/famafrench.csv")$date))
tempTC <- as.numeric(as.Date(tradingCalendar))
tempFC <- c(tempTC[1]:tempTC[length(tempTC)])
tempMC <- na.locf(tempTC[match(tempFC, tempTC)], fromLast=TRUE)
tempLookupCalendar <- data.frame(actual=as.Date(tempFC, origin=EPOCHST),
                                 trading=as.Date(tempMC, origin=EPOCHST))
tempMatched <- tempLookupCalendar[match(events$date,
                                        tempLookupCalendar$actual), "trading"]

# add this to events
events$tradingDate <- as.Date(as.character(tempMatched))

# identifying duplicate date-stock events ---------------------
# where > 1 event for same stock on the same trading date,
# remove as inconsistent
# For example: http://biz.yahoo.com/z/20090320.html
tempDX <- subset(events, select=c(symbol,tradingDate))
events$duplicate <- as.logical(sign(duplicated(tempDX) +
							        duplicated(tempDX, fromLast=TRUE)))

# replacing old symbols with new, where changed
changes <- read.csv(args$changeFile)
# use qdapTools to do a quick lookup and replace
library("qdapTools")
events$symbol <- events$symbol %lc+% changes


# associating stock characteristics to the events
# missing items are discarded by default
stocks <- read.csv(args$stocksFile)
merged <- merge(events, stocks, by="symbol")

# using the ISO specifications for bourse identification,
# merge the Yahoo-given bourse name with the operating MIC
# (this helps to link bourse with market)
biso <- read.csv("DATA/bourse-iso.csv")
merged <- merge(merged,
                data.frame(bourse=biso$bourse, omic=biso$omic),
                by="bourse")

ciso <- read.csv("DATA/country-iso.csv")
merged <- merge(merged,
                data.frame(country=ciso$country, ciso=ciso$code),
                by="country")

if (args$filter) {
    # select events with desired characteristics
    merged <- subset(merged, subset=(
        tradingDate == date &  # level sets of factors are different!
        duplicate == FALSE &
        (omic == "XNYS" | omic == "XNGS" | omic == "XNMS" | omic == "XNCM") &
        (ciso == "USA" | ciso == "CHN")))
}

# sort by symbol and date
merged <- merged[with(merged, order(symbol, date)), ]

# save the selection
write.csv(merged, file=args$mergedFile, row.names=FALSE, quote=TRUE)

# handily calculate the first and last dates for pricegrabber

# -----------------------------------------------------------
# 252 (= 1-year: roughly 4 events prior) + 20 day margin
# in case of varying event date of ev-4,
window1 <- c(-((252*1)+20), -6)
# 1 week before + 1 week after event
window2 <- c(-5, 5)
# -----------------------------------------------------------

calpos1 <- match(min(merged$tradingDate), tradingCalendar)
calpos2 <- match(max(merged$tradingDate), tradingCalendar)
first <- tradingCalendar[[calpos1+window1[[1]]]]
last <- tradingCalendar[[calpos2+window2[[2]]]]

print(paste("recommended ranges for pricegrabber: ",
            "-s '", first, "' ",
            "-e '", last, "'", sep=""))

# typically at this stage, one would run pricegrabber to read and process the csv
