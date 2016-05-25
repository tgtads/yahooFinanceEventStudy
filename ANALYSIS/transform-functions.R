suppressPackageStartupMessages(library("fastmatch"))  # provides fmatch()


get_data <- function(ev.file, pf.dir, L_1.st, L_2.en){

    subset.events <- read.csv(ev.file)
    ev <- data.frame(symbol=subset.events$symbol, date=subset.events$date)
    ev.n <- nrow(ev)

    # obtaining AR for each event and AAR, CAAR for days of subset ==========

    # reading each master price file pertaining to the event's symbol
    rawData <- lapply(ev$symbol, FUN=function(sym){read.csv(paste(pf.dir, "/", sym, ".csv", sep=""))})

    # extract to lists
    date.full <- sapply(rawData, FUN=function(dat){dat$date})
    volume.full <- sapply(rawData, FUN=function(dat){dat$v})
    price.full <- sapply(rawData, FUN=function(dat){dat$p})

    # find position of the line in each item of date.full
    found.line <- sapply(c(1:ev.n), FUN=function(i){fmatch(ev[i,]$date, date.full[[i]])})

    # grab only the relevant range surrounding, and put them into
    # matrices where row is the relative day from t_0
    date <- sapply(c(1:ev.n), FUN=function(i) {
        date.full[[i]][(found.line[i]+L_1.st):(found.line[i]+L_2.en)]
        })
    volume <- sapply(c(1:ev.n), FUN=function(i) {
        volume.full[[i]][(found.line[i]+L_1.st):(found.line[i]+L_2.en)]
        })
    price <- sapply(c(1:ev.n), FUN=function(i) {
        price.full[[i]][(found.line[i]+L_1.st):(found.line[i]+L_2.en)]
        })
    price.lag <- sapply(c(1:ev.n), FUN=function(i) {
        price.full[[i]][(found.line[i]+(L_1.st-1)):(found.line[i]+(L_2.en-1))]
        })
    ror <- 100*(log(price)-log(price.lag)) # rorLog approach

    return(list(date=date, r=ror, v=volume, p=price))
}


make_ff_fit <- function(date.mat, r.mat, factors.df, estimator="ols", L_1){

    # locate, index, and matrize the required model-specific data
    # for each element in the date matrix, find matching row num
    temp.date.vec <- factors.df$date
    temp.match.index <- apply(date.mat, c(1,2), FUN=function(d){fmatch(d, temp.date.vec)})

    # for each variable in the factor dataframe,
    # grab only the matched elements, and put them into matrices.
    # Doing so from vectors is faster!
    temp.rm.vec <- factors.df$rm
    rm.mat <- apply(temp.match.index, c(1,2), FUN=function(m){temp.rm.vec[m]})
    temp.rf.vec <- factors.df$rf
    rf.mat <- apply(temp.match.index, c(1,2), FUN=function(m){temp.rf.vec[m]})
    temp.smb.vec <- factors.df$smb
    smb.mat <- apply(temp.match.index, c(1,2), FUN=function(m){temp.smb.vec[m]})
    temp.hml.vec <- factors.df$hml
    hml.mat <- apply(temp.match.index, c(1,2), FUN=function(m){temp.hml.vec[m]})

    # for each event, aka each column in R.m, applying regression, returning fits and stats
    fit.stats <- lapply(c(1:ncol(date.mat)), FUN=function(i){
        # normally we would not like to compound I(R ~ rf) to (r.rf=(r.mat[,i]-rf.mat[,i]) etc,
        # but the implementation of white.test() requires it.
        # dat <- data.frame(r=r.mat[,i], rm=rm.mat[,i], rf=rf.mat[,i], smb=smb.mat[,i], hml=hml.mat[,i])
        dat <- data.frame(r.rf=(r.mat[,i]-rf.mat[,i]),
                          rm.rf=(rm.mat[,i]-rf.mat[,i]),
                          smb=smb.mat[,i],
                          hml=hml.mat[,i])
        if (estimator == "ols" | estimator == "fgls"){
            reg <- lm(r.rf ~ rm.rf + smb + hml, data=dat[(1:L_1),])
            if (estimator == "fgls"){
                aux.reg <- lm(log(residuals(reg)^2) ~ rm.rf + smb + hml, data=dat[(1:L_1),])
                reg <- lm(r.rf ~ rm.rf + smb + hml, data=dat[(1:L_1),], weights=1/exp(fitted(aux.reg)))
                }
            }
        if (estimator == "iwls"){
            reg <- rlm(r.rf ~ rm.rf + smb + hml, data=dat[(1:L_1),])
            }
        pred <- as.data.frame(predict(reg, interval="confidence", level=0.95, newdata=dat))
        list(fit=pred$fit, lwrCI=pred$lwr, uprCI=pred$upr, sigma=summary(reg)$sigma, reg=reg)
        })

    # merge regression outputs to single vectors and matrices, return as named list
    n.fits <- length(fit.stats)
    er <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$fit})
    ar <- r.mat - er # for convenience
    # lwrCI <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$lwrCI})
    # only if we really want to draw a CI for the CAAR
    # uprCI <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$uprCI})
    # I think that this *could* be bootstrapped, anyway
    sigma <- t(as.matrix(sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$sigma})))
    reg <- lapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$reg})
    resids <- sapply(c(1:n.fits), FUN=function(i){resid(fit.stats[[i]]$reg)})
    return(list(n=n.fits, er=er, ar=ar, rm=rm.mat, sigma=sigma, reg=reg, resids=resids))
}


make_mm_fit <- function(date.mat, r.mat, proxies.df, estimator="ols", L_1){

    temp.date.vec <- proxies.df$date
    temp.match.index <- apply(date.mat, c(1,2), FUN=function(d){fmatch(d, temp.date.vec)})

    temp.rm.vec <- proxies.df$rm
    rm.mat <- apply(temp.match.index, c(1,2), FUN=function(m){temp.rm.vec[m]})

    fit.stats <- lapply(c(1:ncol(date.mat)), FUN=function(i){
        dat <- data.frame(r=r.mat[,i], rm=rm.mat[,i])
        if (estimator == "ols" | estimator == "fgls"){
            reg <- lm(r ~ rm,
                      data=dat[(1:L_1),])
            if (estimator == "fgls"){
                aux.reg <- lm(log(residuals(reg)^2) ~ rm,
                              data=dat[(1:L_1),])
                reg <- lm(r ~ rm, data=dat[(1:L_1),], weights=1/exp(fitted(aux.reg)))
                }
            }
        if (estimator == "iwls"){
            reg <- rlm(r ~ rm, data=dat[(1:L_1),])
            }
        pred <- as.data.frame(predict(reg, interval="confidence", level=0.95, newdata=dat))
        list(fit=pred$fit, lwrCI=pred$lwr, uprCI=pred$upr, sigma=summary(reg)$sigma, reg=reg)
        })
    # merge regression outputs to single vectors and matrices, return as named list
    n.fits <- length(fit.stats)
    er <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$fit})
    ar <- r.mat - er # for convenience
    # lwrCI <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$lwrCI})
    # only if we really want to draw a CI for the CAAR
    # uprCI <- sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$uprCI})
    # I think that this *could* be bootstrapped, anyway
    sigma <- t(as.matrix(sapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$sigma})))
    reg <- lapply(c(1:n.fits), FUN=function(i){fit.stats[[i]]$reg})
    resids <- sapply(c(1:n.fits), FUN=function(i){resid(fit.stats[[i]]$reg)})
    return(list(n=n.fits, er=er, ar=ar, rm=rm.mat, sigma=sigma, reg=reg, resids=resids))
}

unlist_internal <- function(d){as.data.frame(apply(d, 2, unlist))}


# TODO: fix white test!
suppressPackageStartupMessages(c(library("het.test"), # provides whites.htest for heteroskedasticity.
                                 library("lmtest"),  # provides dwtest() bgtest() bptest() waldtest()
                                 library("pracma"))) # provides rmserr stderr


get_fit_stats <- function(fit.list, r, estimator="ols", L_1, T){
    # defaults to ols if not defined
    # unfortunately there are tests that cannot be applied to
    # different regression outputs, so NA is returned if unavailable
    ev.n <- fit.list$n
    f.test <- data.frame(f.test.statistic=rep(NA, ev.n), f.test.p.value=rep(NA, ev.n))
    if (estimator == "ols" | estimator == "fgls"){
        f.test <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
            f.test.stat <- summary(fit.list$reg[[i]])$fstatistic[1]
            f.test.dfm <- summary(fit.list$reg[[i]])$fstatistic[2]
            f.test.dfe <- summary(fit.list$reg[[i]])$fstatistic[3]
            return(data.frame(f.test.statistic=f.test.stat,
                              f.test.p.value=as.numeric(pf(f.test.stat,
                                                             f.test.dfm,
                                                             f.test.dfe,
                                                             lower.tail=FALSE))))
            }))))
        }
    # we will report a rejection, not a failure to reject?

    # the Durbin-Watson test for first-order autocorrelation
    dw.test <- data.frame(dw.test.statistic=rep(NA, ev.n), dw.test.p.value=rep(NA, ev.n))
    if (estimator == "ols" | estimator == "iwls"){
        dw.test <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
            test <- dwtest(fit.list$reg[[i]])
            return(data.frame(dw.test.statistic=test$statistic, dw.test.p.value=test$p.value))
            }))))
        }

#    bg.test <- data.frame(bg.test.statistic=rep(NA, ev.n), bg.test.p.value=rep(NA, ev.n))
    # the Breusch—Godfrey test for greater levels of autocorrelation
     bg.test <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
         test <- bgtest(fit.list$reg[[i]])
         return(data.frame(bg.test.statistic=test$statistic, bg.test.p.value=test$p.value))
         }))))

    # Breusch—Pagan—Koenker test for heteroskedasticity
    bp.test <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
        test <- bptest(fit.list$reg[[i]], studentize=FALSE)
        return(data.frame(bp.test.statistic=test$statistic, bp.test.p.value=test$p.value))
        }))))

    # white test
    white.test <- data.frame(white.test.statistic=rep(NA, ev.n), white.test.p.value=rep(NA, ev.n))
    # if (estimator == "ols" | estimator == "fgls"){
    # white.test <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
    #       test <- whites.htest(fit.list$reg[[i]])
    #       return(data.frame(white.test.statistic=test$statistic, white.test.p.value=test$p.value))
    #       }))))
    # }
    prediction.errors <- unlist_internal(as.data.frame(t(sapply(c(1:ev.n), FUN=function(i){
        rmserr(x=r[(L_1 + 1):T, i],
               y=fit.list$er[(L_1 + 1):T, i])
        })))) # only for the est window

    #    Thiel's uncertainty coefficient
    theils.u <- sapply(c(1:ev.n), FUN=function(i){Yhat_t=fit.list$er[(L_1 + 1):T, i]
                                                  Y_t=r[(L_1 + 1):T, i]
                                                  return(sqrt(mean((Yhat_t - Y_t)^2)) / ( sqrt(mean(Yhat_t^2)) + sqrt(mean(Y_t^2)) ))})

    aic <- sapply(c(1:ev.n), FUN=function(i){as.numeric(AIC(fit.list$reg[[i]]))})
    rss <- sapply(c(1:ev.n), FUN=function(i){as.numeric(deviance(fit.list$reg[[i]]))})
    r.squared <- sapply(c(1:ev.n), FUN=function(i){as.numeric(summary(fit.list$reg[[i]])$r.squared)})
    adj.r.squared <- rep(NA, ev.n)
    if (estimator == "ols" | estimator == "fgls"){
        adj.r.squared <- unlist(lapply(c(1:ev.n), FUN=function(i){as.numeric(summary(fit.list$reg[[i]])$adj.r.squared)}))
        }
    sigma <- sapply(c(1:ev.n), FUN=function(i){as.numeric(summary(fit.list$reg[[i]])$sigma)})

    return(cbind(f.test, dw.test, bg.test, bp.test, white.test,
                 prediction.errors, theils.u, aic, rss,
                 r.squared, adj.r.squared, sigma))
}

get_breakdown <- function(test.df, cval){
    c(H_0.p_lt=nrow(subset(test.df, subset=((statistic <= cval) & (p.value < 0.05)))),
      H_0.p_ge=nrow(subset(test.df, subset=((statistic <= cval) & (p.value >= 0.05)))),
      H_1.p_lt=nrow(subset(test.df, subset=((statistic > cval) & (p.value < 0.05)))),
      H_1.p_ge=nrow(subset(test.df, subset=((statistic > cval) & (p.value >= 0.05)))))
}

summarise_test_results <- function(fit.stats.df, model){

    # TODO: get number of ind vars from fit.stats.df

    # what about H_1 and H_0?
    test <- data.frame(statistic=fit.stats.df$f.test.statistic,
                       p.value=fit.stats.df$f.test.p.value)
    m <- c(p_lt=nrow(subset(test, subset=(p.value < 0.05))),
           p_ge=nrow(subset(test, subset=(p.value >= 0.05))))
    um <- 100*(m/nrow(test))
    f.test.rep <- um


    # this must be implemented so it works for mm and ff!!
    test.df <- data.frame(statistic=fit.stats.df$dw.test.statistic,
                          p.value=fit.stats.df$dw.test.p.value)
    # obs >= 100, \alpha = 0.05
    # based on table of p 844 of Newbold Statistics, originally from DurbinWatson1951
    dL.v <- c(1.65, 1.63, 1.61, 1.59, 1.57)
    dU.v <- c(1.69, 1.72, 1.74, 1.76, 1.78)
    if (model == "ff"){K <- 3}
    if (model == "mm"){K <- 1}
    dL <- dL.v[K]
    dU <- dU.v[K] # where K is number of ind. variables
    m <- c(H_0.p_lt=nrow(subset(test.df, subset=((statistic > dU) & (p.value < 0.05)))),
           H_0.p_ge=nrow(subset(test.df, subset=((statistic > dU) & (p.value >= 0.05)))),
           H_1.p_lt=nrow(subset(test.df, subset=((statistic >= dL)&(statistic <= dU) & (p.value < 0.05)))),
           H_1.p_ge=nrow(subset(test.df, subset=((statistic >= dL)&(statistic <= dU) & (p.value >= 0.05)))),
           NA.p_lt=nrow(subset(test.df, subset=((statistic < dL) & (p.value < 0.05)))),
           NA.p_ge=nrow(subset(test.df, subset=((statistic < dL) & (p.value >= 0.05)))))
    dw.test.rep <- 100*(m/nrow(test.df))

    cval <- qchisq(0.05,2,lower.tail=F)

    # the Breusch—Godfrey test for greater levels of autocorrelation
    test.df <- data.frame(statistic=fit.stats.df$bg.test.statistic,
                          p.value=fit.stats.df$bg.test.p.value)
    bg.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    test.df <- data.frame(statistic=fit.stats.df$bp.test.statistic,
                          p.value=fit.stats.df$bp.test.p.value)
    bp.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    test.df <- data.frame(statistic=fit.stats.df$white.test.statistic,
                          p.value=fit.stats.df$white.test.p.value)
    white.test.rep <- 100*(get_breakdown(test.df, cval=cval)/nrow(test.df))

    return(list(f.test.rep=f.test.rep,
                dw.test.rep=dw.test.rep,
                bg.test.rep=bg.test.rep,
                bp.test.rep=bp.test.rep,
                white.test.rep=white.test.rep))
}

get_wilcox <- function(a, b){
    wilcox.test(coredata(a), coredata(b), paired=TRUE)
    }

get_patell <- function(rm, ar, sigma, w1, w2){ # eq (30)
    L_1.st <- w1[[1]]
    L_1.en <- w1[[2]]
    L_1 <- length(w1[[1]]:w1[[2]])
    L_2 <- length(w2[[1]]:w2[[2]])

    t_1 <- w2[[1]]-1
    tau <- L_2

    AR <- ar
    n.events <- length(sigma)
    t_1.idx <- t_1-(L_1.st)+1
    CAR <- apply(AR[(t_1.idx+1):(t_1.idx+tau),], 2, sum)
    S_CAR <- get_s_car(t_1=t_1, tau=tau, rm=rm, sigma=sigma, L_1=L_1, L_1.st=L_1.st, L_1.en=L_1.en, L_2=L_2)

    # eq (4):
    # all should be of same dimension
    SCAR <- CAR / S_CAR
    statistic <- sqrt((n.events * (L_1-4)) / (L_1-2) ) * mean(SCAR)

    return(list(day=NA, statistic=statistic, p.value=2*(1-pnorm(abs(statistic)))))
    }


get_bmp <- function(rm, ar, sigma, resids, w1, w2){ # eq (30)
    L_1.st <- w1[[1]]
    L_1.en <- w1[[2]]
    L_1 <- length(w1[[1]]:w1[[2]])
    L_2 <- length(w2[[1]]:w2[[2]])

    t_1 <- w2[[1]]-1
    tau <- L_2

    AR <- ar
    n.events <- ncol(AR)
    t_1.idx <- t_1-(L_1.st)+1
    CAR <- apply(AR[(t_1.idx+1):(t_1.idx+tau),], 2, sum)
    S_CAR <- get_s_car(t_1=t_1, tau=tau, rm=rm, sigma=sigma, L_1=L_1, L_1.st=L_1.st, L_1.en=L_1.en, L_2=L_2)
    SCAR <- CAR / S_CAR

    # eq (4):
    # all should be of same dimension
    S_SCAR <- get_s_scar(resids=resids, SCAR=SCAR, clustering=FALSE)
    statistic <- ( mean(SCAR) * sqrt(n.events) ) / S_SCAR

    return(list(day=NA, statistic=statistic, p.value=2*(1-pnorm(abs(statistic)))))
    }



get_grank <- function(rm, ar, sigma, resids, clustering=FALSE, w1, w2){
    L_1.st <- w1[[1]]
    L_1.en <- w1[[2]]
    L_1 <- length(w1[[1]]:w1[[2]])
    L_2 <- length(w2[[1]]:w2[[2]])

    t_1 <- w2[[1]]-1
    tau <- L_2

    AR <- ar

    # t_1 is a relative event day (the day before the CAR period we want to know about),
    # t_1 MUST be >= L_1.en

    # tau is a variable length of event days of interest,
    # tau must be <= L_2

    SAR <- AR / repmat(sigma,nrow(AR),1) ; # eq (2) . SAR is calculated for all days of AR inc the est win

    n.events <- ncol(sigma)
    t_1.idx <- t_1-(L_1.st)+1

    CAR <- apply(AR[(t_1.idx+1):(t_1.idx+tau),], 2, sum) # this returns vector of length n.events
    S_CAR <- get_s_car(t_1=t_1, tau=tau, rm=rm, sigma=sigma, L_1=L_1, L_1.st=L_1.st, L_1.en=L_1.en, L_2=L_2)
    # eq (4)
    # all should be of same dimension
    SCAR <- CAR / S_CAR

    S_SCAR <- get_s_scar(resids=resids, SCAR=SCAR, clustering=FALSE)
    # eq (5), in notation as SCAR*:
    SCAR.std <- SCAR / S_SCAR

    if (tau >= L_2){
        GSAR <- rbind(SAR[1:t_1.idx,], t(SCAR.std)) # eq (8)
    } else {
        # the SAR function fails on out of bounds array ref when tau < L_2
        GSAR <- rbind(SAR[1:t_1.idx,], t(SCAR.std), SAR[(t_1.idx+tau+1):nrow(SAR),]) # eq (8)
    }

    rank.GSAR <- apply(GSAR, 2, rank)
    gT <- L_1+1
    # eq (9):
    U <- (rank.GSAR/(gT+1)) - (1/2)

    # eq (15):
    # daily mean
    mU = apply(U, 1, mean)

    # eq (14), simplified:
    S_mU <- sqrt((1/gT) * sum(mU^2) )
    mU_0 = mU[t_1.idx+1]

    # eq (13):
    Z <- mU_0 / S_mU

    # eq (12):
    GRANK.T <- Z * sqrt((gT-2)/(gT-1-(Z^2)))
    GRANK.T.p.value <- 2 * (1-pnorm(abs(GRANK.T)))
    # this is the T-stat for squeezed (cumulative abnormal returns)
    # event window of length=L_2.
    # while we know the event day, if there is partial clustering
    # this leads me to want to use the T-stat as it is more robust
    # than GRANK-Z under cross-sectional correlation.

    # eq (20):
    # GRANK.Z is appropriate under no clustering and known event day
    GRANK.Z <- sqrt((12*n.events*(gT+1)) / (gT-1) ) * mU_0

    # "Under the null hypothesis of no abnormal returns on the event day,
    # the distribution of the test statistic is asymptotically normal."
    GRANK.Z.p.value <- 2 * (1-pnorm(abs(GRANK.Z)))

    return(list(day=NA, t.statistic=GRANK.T, t.p.value=GRANK.T.p.value,
                       z.statistic=GRANK.Z, z.p.value=GRANK.Z.p.value))
}

# eq (6):
get_s_scar <- function(resids, SCAR, clustering=FALSE){
    n.events <- ncol(resids)
    if (clustering){
        resids.est <- resids[1:L_1,]
        rbar <- get_cross_correlation_m(resids.est)$mean
        # eq (9):
        S_SCAR <- sqrt(((1/(n.events-1)) * sum((SCAR-mean(SCAR))^2 )) / (1 - rbar) )
    } else {
        # eq (6) & eq (7):
        S_SCAR <- sqrt((1/(n.events-1)) * sum((SCAR-mean(SCAR))^2 ) )
    } # singular
    return(S_SCAR)
}


get_s_car <- function(t_1, tau, rm, sigma, L_1, L_1.st, L_1.en, L_2){
    n.events <- ncol(rm)
    t_1.idx <- t_1-(L_1.st)+1
    rm.est <- rm[1:L_1,]
    rm.evt <- rm[(L_1+1):(L_1+L_2),]
    # this again refers to t_1 and tau

    # S_CAR for a (t_1+1):(t_1+tau) event window,
    # by event because X* and X are individual to the event,
    # is the standard deviation of the prediction errors
    # in the cumulative abnormal returns
    S_CAR <- sapply(c(1:n.events), FUN=function(evt.i){
        sigma_i <- sigma[evt.i] # singular
        X.est_i <- cbind(1,rm.est[,evt.i])
        X.evt_i <- cbind(1,rm.evt[,evt.i])
        V_i <- (diag(L_2)*(sigma_i^2))+(X.evt_i %*% solve(crossprod(X.est_i)) %*% t(X.evt_i) * (sigma_i^2))
        # if we are considering the full 'squeezed' period, gamma is all 1's.
        # gamma <- matrix(1, L_2, 1)
        gamma <- matrix(0, L_2, 1)
        rel_t_1 <- t_1 - L_1.en
        # otherwise mark out only the range we want to 'squeeze':
        gamma[(rel_t_1+1):(rel_t_1+tau),] <- 1
        S_CAR_i <- t(gamma) %*% V_i %*% gamma
        return(as.vector(S_CAR_i))
        })

    return(S_CAR)
}

get_student_ar <- function(AAR_t, AR, w1, w2){
    n.events <- ncol(AR)
    statistic <- AAR_t * ( sqrt(n.events) / sqrt((sum((AR - apply(AR, 2, mean))^2)/n.events-1) )    )
    return(list(day=seq(w2[[1]], w2[[2]]), statistic=statistic, p.value=2*(1-pnorm(abs(statistic)))))
}

# eq (27):
get_student_car <- function(CAAR_t, resids, w1, w2){
    L_1 <- length(w1[[1]]:w1[[2]])
    L_2 <- length(w2[[1]]:w2[[2]])

    n.events <- ncol(resids)
    var <- sapply(c(1:n.events), FUN=function(evt.i){
        resids.evt_i <- resids[(nrow(resids)-L_2):L_2,evt.i]
        var_i <- (1 / (L_1-2)) * crossprod(resids.evt_i)
        })
    mvar <- ( 1 / (n.events^2) ) * sum(var)
    CAR.evt.se <- sqrt(mvar)
    statistic <- CAAR_t / CAR.evt.se
    return(list(day=seq(w2[[1]], w2[[2]]), statistic=statistic, p.value=2*(1-pnorm(abs(statistic)))))
}
# the input is the CAAR for day t of the event window,
# and the resids matrix for all events used to form CAAR

suppressPackageStartupMessages(library("gtools")) # provides stars.pval

# finding the significance 'stars' for a vector of values, for different levels and dist types
sig_stars <- function(stat, df, dst="t"){
    if (dst == "t"){
        strs <- stars.pval(pt(abs(stat),df=df,lower.tail=F))
        # alt parameter rej: sigLevel<-0.05
        # rej=(stat > qt((1-sigLevel), df))
        }
    if (dst == "chisq"){
        strs <- stars.pval(pchisq(abs(stat),df=df,lower.tail=F))
    }
    if (dst == "norm"){
        strs <- stars.pval(pnorm(abs(stat),df=df,lower.tail=F))
    }
    return(trimws(paste(stat, strs, sep="")))
}


