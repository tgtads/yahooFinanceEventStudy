#!/usr/bin/Rscript --slave

# calculation of average (and cumulative) abnormal return for n events using
# different return estimation models, and least-squares estimation methods.
# either or both abnormal returns, or model fit statistics
# can be returned (see optional arguments)

# use -h or --help to see usage and options

suppressPackageStartupMessages(library("argparse"))

parser <- ArgumentParser()
parser$add_argument("-r", "--returnsFile", action="store", type="character",
                    help="abnormal return file path, optional", default="")
parser$add_argument("-f", "--fitStatsFile", action="store", type="character",
                    help="model fit statistics file path, optional", default="")
parser$add_argument("-p", "--pricingModel", action="store", type="character",
                    help="pricing model to calculate returns, [ff]|mm", default="ff")
parser$add_argument("-m", "--method", action="store", type="character",
                    help="estimation method, [ols]|fgls|iwls", default="ols")
parser$add_argument("-t", "--testType", action="store", type="character",
                    help="significance test type, [student]|patell|bmp|grank", default="student")
parser$add_argument("-s", "--sigtestFile", action="store", type="character",
                    help="significance test results file path", default="")

parser$add_argument("eventsFile", nargs=1, help="event file path")
parser$add_argument("pricesFolder", nargs=1, help="prices folder path")

args <- parser$parse_args()

# functions for getting, forming or generating data
# kept external for legibility
source("ANALYSIS/transform-functions.R")

# -----------------------------------------------------------
# 252 (= 1-year: roughly 4 events prior) + 20 day margin
# in case of varying event date of ev-4,
window1 <- c(-((252*1)+20), -6)
# 1 week before + 1 week after event
window2 <- c(-5, 5)
# -----------------------------------------------------------

window1Length <- length(window1[[1]]:window1[[2]])
window2Length <- length(window2[[1]]:window2[[2]])
windowsLength <- window1Length + window2Length

# ==================== get and prepare model-specific data
# grab, recalculate the fama-french data
ffFactors <- read.csv("DATA/famafrench-cal.csv")

data <- get_data(ev.file=args$eventsFile, pf.dir=args$pricesFolder,
                 L_1.st=window1[[1]], L_2.en=window2[[2]])

if (args$pricingModel == "ff") {
    indVars <- 3
    fits <- make_ff_fit(date.mat=data$date, r.mat=data$r,
                        factors.df=ffFactors, estimator=args$method,
                        L_1=window1Length)
}

# if we wish to inspect data$r, we will need to export it specially.
# the same goes for data$p or data$v

if (args$pricingModel == "mm") {
    indVars <- 1
    # use a subset of the fama-french data as market model proxies
    mmProxies <- subset(ffFactors, select=c(date, rm))
    fits <- make_mm_fit(date.mat=data$date, r.mat=data$r,
                        proxies.df=mmProxies, estimator=args$method,
                        L_1=window1Length)
}

# write.csv(x=fits$resids, file="resids.csv", row.names=FALSE)

# get and write the derivatives of abnormal return
ar <- data$r - fits$er
relativeDay <- seq(window2[[1]], window2[[2]])
averageVolume <- apply(data$v[(window1Length+1):windowsLength,], 1, mean)
aar <- apply(ar[(window1Length+1):windowsLength,], 1, mean)
caar <- cumsum(aar)

if (nchar(args$returnsFile) > 0) {
    print("writing the abnormal returns")
    write.csv(x=data.frame(relativeDay, averageVolume, aar, caar),
              file=args$returnsFile, row.names=FALSE)
}

if (nchar(args$sigtestFile) > 0) {
    if (args$testType == "student") {
        results <- get_student_car(CAAR_t=caar, resids=fits$resids,
                                   w1=window1, w2=window2)
    }
    if (args$testType == "patell") {
        results <- get_patell(rm=fits$rm, ar=fits$ar, sigma=fits$sigma,
                              w1=window1, w2=window2)
    }
    if (args$testType == "bmp") {
        results <- get_bmp(rm=fits$rm, ar=fits$ar, sigma=fits$sigma,
                           resids=fits$resids, w1=window1, w2=window2)
    }
    if (args$testType == "grank") {
        # obtaining the GRANK-Z significance of the caar on event day
        results <- get_grank(rm=fits$rm, ar=fits$ar, sigma=fits$sigma,
                             resids=fits$resids, clustering=FALSE,
                             w1=window1, w2=window2)
        results$statistic <- results$z.statistic
    }

    testResults <- data.frame(relativeDay=results$day,
                              statistic=sig_stars(stat=results$statistic, df=100, dst="t"))
    print("writing the significance test results")
    write.csv(x=testResults, file=args$sigtestFile, row.names=FALSE)
}


if (nchar(args$fitStatsFile) > 0) {
    # write the fit stats
    fitStats <- get_fit_stats(fit.list=fits, r=data$r, estimator=args$method,
                              L_1=window1Length, T=windowsLength)
    fitStats$indVars <- indVars
    print("writing the fit stats")
    write.csv(x=fitStats, file=args$fitStatsFile, row.names=FALSE)
}
# for another analysis:
# fitSummary <- summarise_test_results(fit.stats.df=fitStats, model=args$pricingModel)
