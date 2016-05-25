#!/usr/bin/Rscript --slave

# script for drawing CAAR and related variables from selected input file

# use -h or --help to see usage and options

# useful, simple function
reindex <- function(data, index){data.frame(index, data)}

suppressPackageStartupMessages(library("argparse"))

parser <- ArgumentParser()
parser$add_argument("-v", "--drawVolume", action="store_true",
                    help="draw average volume", default=FALSE)
parser$add_argument("-a", "--drawAar", action="store_true",
                    help="draw average abnormal return", default=FALSE)
parser$add_argument("-c", "--drawCaar", action="store_true",
                    help="draw cumulative average abnormal return", default=FALSE)
parser$add_argument("inputFile", nargs=1, help="csv file to be read")
parser$add_argument("outputFile", nargs=1, help="pdf file to be returned")

args <- parser$parse_args()


data <- read.csv(args$inputFile)
sig <- sign(min(data$caar))


pdf(args$outputFile)

# prep canvas
par(mfrow=c(1,1), family="Times")

# prepare the area, don't draw anything
plot(reindex(data$caar, data$relativeDay),
     xlab="relative day", ylab="caar",
     xlim=range(data$relativeDay),
     ylim=range(data$caar),
     type="n")

# draw marker lines for day 0 etc
abline(v=0, lty=1, col="gray")
abline(h=max(data$caar), lty=3, col="gray")
abline(h=min(data$caar), lty=3, col="gray")

if (args$drawVolume) {
    points(reindex((data$averageVolume * 1/2000000 * sig), data$relativeDay),
           type="h", pch=20, col="green")
}

if (args$drawAar) {
    points(reindex(data$aar, data$relativeDay),
           type="l", pch=20, col="black")
    points(reindex(data$aar, data$relativeDay),
           type="p", pch=20, col="green")
}

if (args$drawCaar) {
    points(reindex(data$caar, data$relativeDay),
           type="l", pch=20, col="black")
    points(reindex(data$caar, data$relativeDay),
           type="p", pch=20, col="red")
}

dev.off()  # save pdf

