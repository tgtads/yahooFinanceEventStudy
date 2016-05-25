#!/usr/bin/Rscript --slave

# script for reporting on the normality of data passed in a csv

# use -h or --help to see usage and options

suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("pracma")) # provides rmserr stderr
suppressPackageStartupMessages(library("moments")) # provides skewness()

parser <- ArgumentParser()
parser$add_argument("inputFile", nargs=1, help="data file path (csv)")
parser$add_argument("outputFile", nargs=1, help="summary statistic file path")
parser$add_argument("outputPDF", nargs=1, help="pdf file path")
parser$add_argument("figNumber", nargs=1, help="pdf figure number")
args <- parser$parse_args()

data <- read.csv(args$inputFile)

# for each variable in data,
# make_report()
# describe_normality()

# library(pracma) # provides rmserr stderr
# library(moments)  # provides skewness()

make_report <- function(vec){
    if (length(vec) <= 5000){normTest <- shapiro.test(vec)}
    else{normTest <- shapiro.test(sample(vec, 5000))} # limited to 5000
    return(data.frame(mean=mean(vec),
                      stderr=std_err(vec),
                      Q1=as.numeric(quantile(vec)[2]),
                      median=median(vec),
                      Q3=as.numeric(quantile(vec)[4]),
                      sd=sd(vec),
                      sample.variance=var(vec),
                      kurtosis=kurtosis(vec),
                      skewness=skewness(vec), # preferred over jarque-bera test
                      normTest.stat=as.numeric(normTest[1]),
                      normTest.p.val=as.numeric(normTest[2]),
                      min=min(vec),
                      max=max(vec),
                      IQR=IQR(vec),
                      sum=sum(vec),
                      count=length(vec)))
    }

check_type <- function(v) {
   if (is.null(levels(v))) {
        if (typeof(v) == "double" | typeof(v) == "integer") {
            return(v)
        }
    }
}

clean_df <- function(df) {
    newDf <- as.data.frame(do.call(cbind, sapply(data, check_type)))
    return(newDf)
}

data <- clean_df(data)

# (a better approach would be to reform the dataframe by removing non-double)

# for each numeric column,
reports <- t(do.call(rbind, lapply(data, make_report)))

# save it
write.csv(x=reports, file=args$outputFile)


aspectRatio=sqrt(2)
xdim=9/aspectRatio
# universal variable for A4 proportions

describe_normality <- function(sets, figNumber, rows, cols, qqceil, orient="p", savePDF=FALSE, filename){ # works for n sets of matrices
    if(orient == "p"){width <- xdim ; height <- xdim*aspectRatio}
    if(orient == "l"){height <- xdim ; width <- xdim*aspectRatio}
    if(savePDF==TRUE){pdf(filename, width=width, height=height)}
    par(mfrow=c(rows,cols), mai=c(0.55,0.55,0.55,0.55), family="Times") # prep canvas
    for (i in 1:length(sets)) {
        vec <- as.vector(sets[[i]])

        name <- paste("Fig ", figNumber, "-", (i*2)-1, " (", names(sets[i]), ")", sep="")
        hist(vec, 20, prob=TRUE, main=name, xlab="Data", border="blue")
        lines(density(vec), lty=2)
        curve(dnorm(x, mean=mean(vec), sd=sd(vec)), add=TRUE, lty=3)

        name <- paste("Fig ", figNumber, "-", (i*2), " (", names(sets[i]), ")", sep="")
        if (length(vec) > qqceil) {
            qqvec <- sample(vec, qqceil, replace=FALSE)
        } else {
            qqvec <- vec
        }
        qqnorm(qqvec, pch=".", main=name, col="blue")
        qqline(qqvec, lty=3)
    }
    if(savePDF==TRUE){dev.off()}  # save pdf
}

# draw function
describe_normality(figNumber=args$figNumber, rows=3, cols=2, qqceil=(278000*0.05), orient="p",
                   filename=args$outputPDF, savePDF=TRUE,
                   sets=data)


