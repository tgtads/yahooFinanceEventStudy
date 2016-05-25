#!/usr/bin/Rscript --slave

# script for creating simulated events using a random selection of stocks

# USAGE:
# Rscript PREPROCESSING/simulate_events.R


sample.df <- function(df, n){df[sample(nrow(df), n, replace=TRUE), , drop=FALSE]}

n <- 30000

# take the profile list

selectedProfiles <- sample.df(read.csv("DATA/SIM/profiles.csv"), n)

# randomly select dates from 2015
cal <- data.frame(date=read.csv("DATA/famafrench.csv")$date)
cal$year <- substring(cal$date, 0, 4)
dates <- sample.df(subset(cal, year==2015), n)$date

realEvents <- sample.df(read.csv("DATA/MAIN/events.csv"), n)

events <- data.frame(date=dates,
					 symbol=selectedProfiles$symbol,
					 name=selectedProfiles$name,
					 surprise=realEvents$surprise,
					 eps=realEvents$eps,
					 eeps=realEvents$eeps)

write.csv(events, "DATA/SIM/events.csv", row.names=FALSE)