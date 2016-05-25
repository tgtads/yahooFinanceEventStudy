#!/usr/bin/Rscript --slave

# this loads the fama-french factors
# and carries forward the previous observations for
# any calendar days where there are none

# USAGE:
# Rscript PREPROCESSING/complete_factors.R

library(zoo) # provides na.locf()

EPOCHST <- "1970-01-01"

ffFactors <- read.csv("DATA/famafrench.csv")

# ensure we have factors for the range of date events.
# if in doubt, re-obtain from prof. french's website.
ffFactorDates <- ffFactors$date
first <- as.numeric(as.Date(ffFactors$date[1]))
last <- as.numeric(as.Date(ffFactors$date[length(ffFactors$date)]))
# form a full calendar
fc <- merge(x=data.frame(date=as.factor(as.Date(x=c(first:last), origin=EPOCHST))),
            y=ffFactors,
            by="date",
            all=TRUE)

ffFactorsCarriedForward <- data.frame(fc$date, na.locf(fc[,2]), na.locf(fc[,3]), na.locf(fc[,4]), na.locf(fc[,5]))
colnames(ffFactorsCarriedForward) <- colnames(ffFactors) # fix col names

# recalculate rm
ffFactorsCarriedForward$rm <- ffFactorsCarriedForward$rmrf + ffFactorsCarriedForward$rf

write.csv(ffFactorsCarriedForward, file="DATA/famafrench-cal.csv", row.names=FALSE)