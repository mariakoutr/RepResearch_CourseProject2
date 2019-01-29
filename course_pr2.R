course_pr2 <- function(){
## Load and read data in R

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "data.bz2", method = "curl")
data <- read.csv(bzfile("data.bz2")) 

## Load the libraries  
library(knitr)
library(markdown)
library(rmarkdown)
library(plyr)
library(stats)
library(ggplot2)
library(reshape2)

## Subset the data 
dataUse<- data[,c("EVTYPE", "FATALITIES","INJURIES","PROPDMG",
                  "PROPDMGEXP","CROPDMG","CROPDMGEXP")]

## Calculate property and crop damage values  
dataUse$PROPDMGEXP <- as.character(dataUse$PROPDMGEXP) 
dataUse$CROPDMGEXP <- as.character(dataUse$CROPDMGEXP)
dataUse$PROPDMGEXP <- mapvalues(dataUse$PROPDMGEXP,
                        from = c("" , "-","?" ,"+", "0", "1",
                         "2", "3", "4", "5", "6", "7","8", "B", "h",
                         "H", "K","m","M"), to=c("0","0","0","1","10",
                        "10","10","10","10","10","10","10","10",
                        "1000000000","100","100","1000","1000000",
                        "1000000"))
dataUse$PROPDMGEXP<- as.numeric(dataUse$PROPDMGEXP)
dataUse$PROPDMGVAL <- dataUse$PROPDMG*dataUse$PROPDMGEXP

dataUse$CROPDMGEXP <- mapvalues(dataUse$CROPDMGEXP, 
                                from = c("" , "M" ,"K", "m", "B", "?",
                                "0", "k", "2"), to = c("0","1000000",
                                "1000","1000000","1000000000","0","10",
                                "1000","10"))
dataUse$CROPDMGEXP<- as.numeric(dataUse$CROPDMGEXP)
dataUse$CROPDMGVAL <- dataUse$CROPDMG*dataUse$CROPDMGEXP

## Health Impact of weather events
## Total number of fatalitites and injuries per event 
fatalitiesSum <- aggregate(FATALITIES ~ EVTYPE, data = dataUse, sum)
injuriesSum <- aggregate(INJURIES ~ EVTYPE, data = dataUse, sum)
fatalitiesSum <- fatalitiesSum[order(fatalitiesSum$FATALITIES, decreasing = TRUE),]
injuriesSum <- injuriesSum[order(injuriesSum$INJURIES, decreasing = TRUE),]

## Plot the number of fatalities and injuries for the events 
## with the 10 highest number of fatalities and injuries respectively.
png("fat_inj.png", height = 800, width = 800, unit = "px")
par(mfrow = c(1,2), mar= c(10,4,0,0), oma = c(2,0,2,0))
barplot(fatalitiesSum$FATALITIES[1:10], names.arg = fatalitiesSum$EVTYPE[1:10],las = 3, ylab = "Number of fatalities")
barplot(injuriesSum$INJURIES[1:10], names.arg = injuriesSum$EVTYPE[1:10],las = 3, ylab = "Number of injuries")
title("Top 10 events with the highest number of fatalities and injuries in the U.S.", outer = TRUE)
mtext('Event type', side = 1, outer = TRUE, line =1)
dev.off()

## Plot the top 10 event types with the highest 
## economic damage (property + crop)
dataUse$DMGTOTAL <- dataUse$PROPDMGVAL + dataUse$CROPDMGVAL
economicdata <- aggregate(cbind(PROPDMGVAL,CROPDMGVAL,DMGTOTAL)~ EVTYPE, data = dataUse, sum)
economicdata <- economicdata[order(economicdata$DMGTOTAL,decreasing = TRUE),]
ecdatatop10 <- economicdata[1:10,]

economicDataTopMelt <- melt(ecdatatop10, id.vars="EVTYPE")
png("ecdamage.png")
gg <- ggplot(economicDataTopMelt, aes(x=reorder(EVTYPE,-value), y = value))
gg <- gg + geom_bar(stat = "identity", aes(fill = variable), position = "dodge")
gg <- gg + scale_y_sqrt("Economic Damage [$]")
gg <- gg + xlab("Event type")
gg <- gg + theme(axis.text.x = element_text(angle=45, hjust=1))
gg <- gg + ggtitle("Top 10 event types with the highest economic damage value in the U.S. ")
plot(gg)
dev.off()
}