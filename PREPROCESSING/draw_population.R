# tools for drawing, for execution in the R environment

# ============================================================
# DATA ANALYSIS

# Showing the population of events over time

# there is an even more granular version of the event-growth graph in formingSubsetsQuicker.R.


library(ggplot2)
library(reshape) # provides melt()

get_quarter <- function(month){
  quarter <- switch(month,
                    "1"=1, "2"=1, "3"=1,
                    "4"=2, "5"=2, "6"=2,
                    "7"=3, "8"=3, "9"=3,
                    "10"=4, "11"=4, "12"=4, NA)
  return(quarter)
}

# a palette compatible with both color and grayscale printing
colorblindPalette <- c("#CC79A7", "#0072B2", "#F0E442", "#999999",
                       "#56B4E9", "#009E73", "#E69F00", "#D55E00")

# standard theme that fits with latex-made pdfs
latexTheme <- theme(text=element_text(size=12, family="Times"))

aspectRatio <- sqrt(2)

simple_melt <- function(data){

  # data <- events

  # this function reforms the events data
  # and draws a stacked graph of events by ciso over time

  # break down the date into groups for presentation
  data$year <- as.POSIXlt(data$date)$year+1900
  data$month <- as.POSIXlt(data$date)$mon+1
  data$quarter <- sapply(data$month, get_quarter)
  data$yearquarter <- paste(data$year, data$quarter, sep="-Q")
  # remove levels from data$ciso
  data$ciso <- as.character(data$ciso)

  # REFORM for PRESENTATION

  data.tb <- table(data$ciso, data$yearquarter)
  table1 <- t(as.matrix(unclass(data.tb)))

  # Unused visually, useful as csv
  # table2 <- as.data.frame(data.tb)
  # colnames(table2) <- c("ciso", "yearquarter", "freq")

  summ <- apply(table1, 2, sum)
  ord <- table1[,rev(order(summ))]
  # includes a group for "all others" for all less than 100
  # includes the group for "unknown"
  reform <- cbind(ord[,1:4], other=apply(ord[,5:ncol(ord)], 1, sum))
  reform.melt <- melt(reform)
  colnames(reform.melt) <- c("year.quarter", "ciso", "frequency")

  return(reform.melt)
}



bourse_table <- function(data, pdfFilename, savePDF=FALSE){

  isoRef <- read.csv("DATA/bourse-iso.csv")

  data <- merge(data, unique(data.frame(omic=isoRef$mic, bciso=isoRef$bciso)), by="omic")
  data$countryBourse <- paste(data$bciso, data$omic, sep="/")

  # table1 <- as.matrix(table(unclass(data$ciso), unclass(data$countryBourse)))
  tempTable <- as.data.frame(table(data$ciso, data$countryBourse))
  colnames(tempTable) <- c("country", "bourse", "frequency")
  tt.df.allevents <- subset(rev(tempTable), subset=(frequency != 0))

  return(tt.df.allevents)
}

frequency_table <- function(ev.d){

  ev.d$year <- as.POSIXlt(ev.d$date)$year+1900
  ev.d$month <- as.POSIXlt(ev.d$date)$mon+1
  ev.d$quarter <- sapply(ev.d$month, get_quarter)
  ev.d$yearQuarter <- paste(ev.d$year, ev.d$quarter, sep="-Q")

  ev.d$compound <- paste(ev.d$ciso, ev.d$sign, sep=",")


  ev.d.tb2.df <- as.data.frame(table(ev.d$compound, ev.d$yearQuarter))
  colnames(ev.d.tb2.df) <- c("set", "yearQuarter", "frequency")

  return(ev.d.tb2.df)
}




events <- read.csv(file="DATA/MAIN/merged-unfiltered.csv")



myMelt <- simple_melt(data=events)

pdf("DOCUMENTATION/ciso-by-time.pdf", width=9, height=(9/aspectRatio))
ggplot(myMelt, aes(x=year.quarter, y=frequency,
                        group=ciso, fill=ciso,
                        order=-as.numeric(as.factor(ciso)))) +
  geom_area(position="stack") +
  scale_fill_manual(values=colorblindPalette) +
  latexTheme +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  ggtitle("Fig 1-1")
dev.off()  # save pdf




myTable <- bourse_table(data=events)

pdf("DOCUMENTATION/evs-by-bourse.pdf", width=4, height=(20/aspectRatio))
ggplot(myTable, aes(x=bourse,y=country)) +
  geom_point(size=5, aes(alpha=frequency^(1/3))) + # root affects the alpha scaling
  # scale_colour_gradient(low="blue", high="red") +
  # scale_colour_gradient(low=muted("blue"), high="red") +
  # scale_size_area(max_size=50) +
  guides(alpha=FALSE) +
  scale_x_discrete(limits=rev(levels(myTable$countryBourse)) ) +
  latexTheme +
  theme(panel.background=element_blank(),
        text=element_text(family="Times"),
        panel.grid.major=element_line(colour="#f0f0f0"),
        axis.ticks=element_line(colour="#f0f0f0"),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        axis.text=element_text(colour="black")) +
  ggtitle("Fig 1-2")
dev.off() # save pdf


events <- read.csv(file="DATA/MAIN/SUBSETS/A.csv")

myTable <- frequency_table(ev.d=events)

pdf("DOCUMENTATION/setA-usable-by-quarter.pdf", width=9, height=(9/aspectRatio))
ggplot(myTable, aes(x=yearQuarter, y=frequency,
                    group=set, fill=set,
                    order=-as.numeric(as.factor(set)))) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=colorblindPalette) +
  latexTheme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
# guides(fill=guide_legend(title=NULL)) +
  ggtitle("Fig 1-3")
dev.off() # save pdf


# useful for csv
# kp <- t(as.matrix(unclass(table(data$ciso, data$mic))))
# write.csv(kp, file="test-all-evs-by-countrymarkets.csv")




