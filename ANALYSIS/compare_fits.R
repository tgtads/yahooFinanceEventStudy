#!/usr/bin/Rscript --slave

# script to compare two "fit files" with identical no of observations

# use -h or --help to see usage and options

suppressPackageStartupMessages(library("argparse"))

parser <- ArgumentParser()
parser$add_argument("fileA", nargs=1, help="initial file to be read")
parser$add_argument("fileB", nargs=1, help="other file to be read")
parser$add_argument("outputFile", nargs=1, help="csv file to be returned")

args <- parser$parse_args()

# both files must be of the same dimensions

summarise_test_results <- function(fit.stats.object, indVars){

    # what about H_1 and H_0?
    test <- data.frame(statistic=fit.stats.object$f.test.statistic,
                       p.value=fit.stats.object$f.test.p.value)
    m <- c(p_lt=nrow(subset(test, subset=(p.value < 0.05))),
           p_ge=nrow(subset(test, subset=(p.value >= 0.05))))
    um <- 100*(m/nrow(test))
    f.test.rep <- um

    # TODO: this must be implemented so it works for both types of model
    test.df <- data.frame(statistic=fit.stats.object$dw.test.statistic,
                          p.value=fit.stats.object$dw.test.p.value)
    # obs >= 100, \alpha = 0.05
    # based on table of p 844 of Newbold Statistics, originally from DurbinWatson1951
    dL.v <- c(1.65, 1.63, 1.61, 1.59, 1.57)
    dU.v <- c(1.69, 1.72, 1.74, 1.76, 1.78)
    dL <- dL.v[indVars]
    dU <- dU.v[indVars] # where indVars is number of ind. variables in the estimation model
    m <- c(H_0.p_lt=nrow(subset(test.df, subset=((statistic > dU) & (p.value < 0.05)))),
           H_0.p_ge=nrow(subset(test.df, subset=((statistic > dU) & (p.value >= 0.05)))),
           H_1.p_lt=nrow(subset(test.df, subset=((statistic >= dL)&(statistic <= dU) & (p.value < 0.05)))),
           H_1.p_ge=nrow(subset(test.df, subset=((statistic >= dL)&(statistic <= dU) & (p.value >= 0.05)))),
           NA.p_lt=nrow(subset(test.df, subset=((statistic < dL) & (p.value < 0.05)))),
           NA.p_ge=nrow(subset(test.df, subset=((statistic < dL) & (p.value >= 0.05)))))
    dw.test.rep <- 100*(m/nrow(test.df))

    cval <- qchisq(0.05,2,lower.tail=F)

    # the Breusch—Godfrey test for greater levels of autocorrelation
    test.df <- data.frame(statistic=fit.stats.object$bg.test.statistic,
                          p.value=fit.stats.object$bg.test.p.value)
    bg.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    test.df <- data.frame(statistic=fit.stats.object$bp.test.statistic,
                          p.value=fit.stats.object$bp.test.p.value)
    bp.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    test.df <- data.frame(statistic=fit.stats.object$white.test.statistic,
                          p.value=fit.stats.object$white.test.p.value)
    white.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    return(list(f.test.rep=f.test.rep,
                 dw.test.rep=dw.test.rep,
                 bg.test.rep=bg.test.rep,
                 bp.test.rep=bp.test.rep,
                 white.test.rep=white.test.rep))
    }


get_breakdown <- function(test.df, cval){
    c(H_0.p_lt=nrow(subset(test.df, subset=((statistic <= cval) & (p.value < 0.05)))),
      H_0.p_ge=nrow(subset(test.df, subset=((statistic <= cval) & (p.value >= 0.05)))),
      H_1.p_lt=nrow(subset(test.df, subset=((statistic > cval) & (p.value < 0.05)))),
      H_1.p_ge=nrow(subset(test.df, subset=((statistic > cval) & (p.value >= 0.05)))))
}

compare_pairs <- function(vec1, vec2, higher=TRUE){
    pair.temp <- cbind(vec1, vec2)
    winner.temp <- unlist(apply(pair.temp, 1, max))
    rows.temp <- as.list(c(1:nrow(pair.temp)))
    winningCombo <- unlist(lapply(rows.temp, FUN=function(r){match(winner.temp[r], pair.temp[r,])}))
    # gives "1" if vec1[i]>vec2[i] and "2" if vec2[i]>vec1[i]
    if (higher) {
        vec1win <- 100-(100*(sum(winningCombo-1) / length(vec1)))
    } else {
        vec1win <- 100*(sum(winningCombo-1) / length(vec1))
    }
    # returns proportion of times value of vec1 'beat' value of vec2
    return(c(vec1win, 100-vec1win))
}

dataA <- read.csv(args$fileA)
dataB <- read.csv(args$fileB)

res <- data.frame(adj.r.squared.higher=compare_pairs(vec1=dataA$adj.r.squared,
                                                     vec2=dataB$adj.r.squared, higher=TRUE),
                  theils.u.lower=compare_pairs(vec1=dataA$theils.u,
                                               vec2=dataB$theils.u, higher=FALSE),
                  aic.lower=compare_pairs(vec1=dataA$aic,
                                          vec2=dataB$aic, higher=FALSE),
                  rmse.lower=compare_pairs(vec1=dataA$rmse,
                                           vec2=dataB$rmse, higher=FALSE),
                  mse.lower=compare_pairs(vec1=dataA$mse,
                                          vec2=dataB$mse, higher=FALSE),
                  mae.lower=compare_pairs(vec1=dataA$mae,
                                          vec2=dataB$mae, higher=FALSE),
                  sigma.lower=compare_pairs(vec1=dataA$sigma,
                                            vec2=dataB$sigma, higher=FALSE))


fitSummaryA <- summarise_test_results(fit.stats.object=dataA, indVars=dataA$indVars[[1]])
fitSummaryB <- summarise_test_results(fit.stats.object=dataB, indVars=dataB$indVars[[1]])

ts <- cbind(unlist(fitSummaryA), unlist(fitSummaryB))

tec <- cbind(res, as.data.frame(t(ts)))
rownames(tec) <- c(args$fileA, args$fileB)

write.csv(x=t(tec), file=args$outputFile, row.names=TRUE)



