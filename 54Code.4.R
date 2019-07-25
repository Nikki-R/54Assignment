url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "stormdata.bz2")
library(R.utils)

bunzip2("stormdata.bz2", "stormdata.csv", remove=FALSE)
stormdata <- read.csv("stormdata.csv", )

library(dplyr)

#Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?

#Very initial analysis
#names(stormdata) 
#unique(stormdata$STATE)
#unique(stormdata$EVTYPE)
#summary(stormdata$EVTYPE)
#max(stormdata$INJURIES) 
#stormdata[which.max(stormdata$INJURIES),]
#stormdata[which.max(stormdata$FATALITIES),]

#Subset by date (48 events -from 1996 onwards)
str(stormdata)
head(stormdata$BGN_DATE)
stormdata$BGN_DATE <- as.Date(stormdata$BGN_DATE, format="%m/%d/%Y")
str(stormdata)

recent <-  stormdata[stormdata$BGN_DATE >= "1996-01-01",]
head(recent)
names(recent)
recent <- select(recent, STATE__, BGN_DATE,END_DATE, COUNTY, STATE, EVTYPE, FATALITIES, INJURIES,
                 PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
head(recent)

#Clean event type
unique(recent$EVTYPE)
recent <- mutate(recent, eventtype = toupper(trimws(EVTYPE)))
recent$eventtype <- gsub("TSTM", "THUNDERSTORM", x=recent$eventtype)
recent$eventtype <- gsub("(G35)", "", x=recent$eventtype)
recent$eventtype <- gsub("(G45)", "", x=recent$eventtype)
recent$eventtype <- gsub("HYPERTHERMIA", "HYPOTHERMIA", x=recent$eventtype)
recent$eventtype <- gsub("URBAN/SML STREAM FLD", "FLOOD", x=recent$eventtype)
recent$eventtype <- gsub("WILD/FOREST FIRE", "WILDFIRE", x=recent$eventtype)
recent$eventtype <- gsub("WINTER WEATHER/MIX", "WINTER WEATHER", x=recent$eventtype)
recent$eventtype <- gsub("^EXTREME COLD$", "EXTREME COLD/WIND CHILL", x=recent$eventtype)
recent$eventtype <- gsub("COLD TEMPERATURE", "COLD/WIND CHILL", x=recent$eventtype)
recent$eventtype <- gsub("EXTENDED COLD", "COLD/WIND CHILL", x=recent$eventtype)
recent$eventtype <- gsub("FOG", "DENSE FOG", x=recent$eventtype)
recent$eventtype <- gsub("LANDSLIDE", "DEBRIS FLOW", x=recent$eventtype)
recent$eventtype <- gsub("RIP CURRENTS", "RIP CURRENT", x=recent$eventtype)
recent <- mutate(recent, eventtype = toupper(trimws(eventtype)))
recent <- mutate(recent, eventtype=as.factor(eventtype))

#Narrow down to key death events

dataevents <- group_by(recent, eventtype)
head(dataevents)
summary(dataevents)
poprisk <- summarise(dataevents, meandeath =mean(FATALITIES),
                       totaldeath=sum(FATALITIES), meaninj=mean(INJURIES), 
                     totalinj=sum(INJURIES))
#Only keep those where neither is 0
poprisk <- mutate(poprisk, totalflag = "")
for (i in (1:nrow(poprisk))){
        if(sum(poprisk[i,2:5])==0){
                poprisk[i,6] <- 0} else{
                        poprisk[i,6] <- 1
        }
}
poprisk<- poprisk[poprisk$totalflag=="1",]
poprisk

meandeathrisk <- arrange(poprisk, desc(meandeath))[1:10,1]
totaldeathrisk<- arrange(poprisk, desc(totaldeath))[1:10,1]
meaninjrisk <- arrange(poprisk, desc(meaninj))[1:10,1]
totalinjrisk<- arrange(poprisk, desc(totalinj))[1:10,1]

#combine and get unique
allrisks <- c(meandeathrisk, totaldeathrisk, meaninjrisk, totalinjrisk)
keyevents <- unlist(allrisks)
keyevents <- (unique(keyevents))

#Then filter table, and visualise
evtable <- poprisk[poprisk$eventtype %in% keyevents,]
evtable
evtable$eventtype <- droplevels(evtable$eventtype)

#Order levels for death
levorder1 <- evtable$eventtype[order(evtable$totaldeath, decreasing = TRUE)]
evtable1 <- evtable
evtable1$eventtype <- factor(evtable1$eventtype, levels=levorder1)
evtable1

#Order levels for injury
levorder2 <- evtable$eventtype[order(evtable$totalinj, decreasing = TRUE)]
evtable2 <- evtable
evtable2$eventtype <- factor(evtable2$eventtype, levels=levorder2)
evtable2

#Remove 0 values for death
evtable1 <- evtable1[evtable1$totaldeath !=0,]
#Remove 0 values for injury
evtable2 <- evtable2[evtable2$totalinj !=0,]

#Visualise deaths
par(mar = c(9,5,2,6))
with(evtable1, plot.default(eventtype, totaldeath, type="p", pch=16, col="blue", axes=FALSE, ylab="Total deaths 1996-2011", xlab=""))
axis(side = 1, at = as.numeric(evtable1$eventtype), labels = evtable1$eventtype, ylim=2000, las=2, xlab="")
axis(side=2)
par(new = T)
with(evtable1, plot.default(eventtype, meandeath, type="p", axes=F, xlab=NA, ylab=NA, cex=1.2, pch=4))
axis(side=4)
mtext(side = 4, line = 3, 'Mean deaths 1996-2011')
box()
legend("topright",
       legend=c("Total deaths", "Mean deaths"),
       pch=c(16, 4), col=c("blue", "black"))


#Visualise injuries
par(mar = c(9,5,2,6))
with(evtable2, plot.default(eventtype, totalinj, type="p", pch=16, col="blue", axes=FALSE, ylab="Total injuries 1996-2011", xlab=""))
axis(side = 1, at = as.numeric(evtable2$eventtype), labels = evtable2$eventtype, ylim=2000, las=2, xlab="")
axis(side=2)
par(new = T)
with(evtable2, plot.default(eventtype, meaninj, type="p", axes=F, xlab=NA, ylab=NA, cex=1.2, pch=4))
axis(side=4)
mtext(side = 4, line = 3, 'Mean injuries 1996-2011')
box()
legend("topright",
       legend=c("Total injuries", "Mean injuries"),
      pch=c(16, 4), col=c("blue", "black"))

#Across the US, which types of events have the greatest economic consequences?
names(recent)

#Calculate total value -ignore "+ [blank] ? -" as documentation is not clear.
#Numbers, 10^x and H - hundred (2), K - thousand (3), M - million (6), B - billion (9)

exponent <- recent
unique(exponent$CROPDMGEXP)
unique(exponent$PROPDMGEXP)
exponent$PROPDMGEXP <- gsub("h", "2", x=exponent$PROPDMGEXP, ignore.case=TRUE)
exponent$PROPDMGEXP <- gsub("k", "3", x=exponent$PROPDMGEXP, ignore.case=TRUE)
exponent$PROPDMGEXP <- gsub("m", "6", x=exponent$PROPDMGEXP, ignore.case=TRUE)
exponent$PROPDMGEXP <- gsub("b", "9", x=exponent$PROPDMGEXP, ignore.case=TRUE)
exponent$CROPDMGEXP <- gsub("h", "2", x=exponent$CROPDMGEXP, ignore.case=TRUE)
exponent$CROPDMGEXP <- gsub("k", "3", x=exponent$CROPDMGEXP, ignore.case=TRUE)
exponent$CROPDMGEXP <- gsub("m", "6", x=exponent$CROPDMGEXP, ignore.case=TRUE)
exponent$CROPDMGEXP <- gsub("b", "9", x=exponent$CROPDMGEXP, ignore.case=TRUE)
unique(exponent$CROPDMGEXP)
unique(exponent$PROPDMGEXP)

# Ensure exponents make sense
class(exponent$CROPDMGEXP)
exponent$CROPDMGEXP <- as.numeric(exponent$CROPDMGEXP)
exponent$PROPDMGEXP <- as.numeric(exponent$PROPDMGEXP)

#Narrow down to events (only with impact in millions or billions)
location <- which(exponent$CROPDMGEXP>=6| exponent$PROPDMGEXP>=6)
location
subset <- exponent[location,]

#Events total

#Calculate exponent
values1 <- mutate(subset, PROPVALUE = as.numeric(""))

for (i in 1:nrow(values1)){
if(is.na(values1$PROPDMGEXP[i])){
  values1$PROPVALUE[i]<-0}else{
    values1$PROPVALUE[i]<-values1$PROPDMG[i]*10^(values1$PROPDMGEXP[i])
  }
}
head(values1)

values2 <- mutate(values1, CROPVALUE =as.numeric(""))
for (i in 1:nrow(values2)){
  if(is.na(values2$CROPDMGEXP[i])){
    values2$CROPVALUE[i]<-0}else{
      values2$CROPVALUE[i]<-values2$CROPDMG[i]*10^(values2$CROPDMGEXP[i])
    }
}

head(values2)

#Summarise cost

summarycost <- group_by(values2, eventtype)
head(summarycost)
summary(summarycost)
costs <- summarise(summarycost, meancropcost =mean(CROPVALUE),
                     totalcropcost=sum(CROPVALUE), meanpropcost=mean(PROPVALUE), 
                     totalpropcost=sum(PROPVALUE), totalcost = sum(CROPVALUE, PROPVALUE))
head(costs)
#Visualise - proportion of cost by event type, against proportion of total number of events by event type
costs <- arrange(costs, desc(totalcost))

cost10 <- costs[1:10,]
cost10$eventtype <- droplevels(cost10$eventtype)

#Order levels for cost
levorder3 <- cost10$eventtype[order(cost10$totalcost, decreasing = TRUE)]
cost10$eventtype <- factor(cost10$eventtype, levels=levorder3)
cost10

stk <- data.frame(cost10[1], stack(cost10[c(3,5)]))
stk <- mutate(stk, Cost= values/10^9)
names(stk)
names(stk) <- c("eventtype", "values", "ind", "valuescaled")

library(ggplot2)

require(scales)
plot <- ggplot() + geom_bar(aes(y = valuescaled, x = eventtype, fill=ind),data = stk,stat="identity")
plot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x="Event", y="Cost (billion dollars)") + scale_y_continuous(labels = comma)

#Proportion of events
#Proportion of cost
costs
proportioncost <- mutate(costs, proportion=as.numeric(""))
for (i in 1:nrow(proportioncost)){
  proportioncost$proportion[i] <- proportioncost$totalcost[i]/sum(proportioncost$totalcost)
}
proportioncost

summarycost
eventnumber <- summarise(summarycost, count=n())
proportionevent <- mutate(eventnumber, proportion=as.numeric(""))
for (i in 1:nrow(proportionevent)){
  proportionevent$proportion[i] <- proportionevent$count[i]/sum(proportionevent$count)
}

#Order
proportionevent <- proportionevent[match(proportioncost$eventtype, proportionevent$eventtype),]
proportionevent

#Order levels for proportion
levorder4 <- proportioncost$eventtype[order(proportioncost$proportion, decreasing = TRUE)]
proportioncost$eventtype <- factor(proportioncost$eventtype, levels=levorder4)
proportionevent$eventtype <- factor(proportionevent$eventtype, levels=levorder4)

proportionevent

costsub <- proportioncost[1:10,]
eventsub <- proportionevent[1:10,]

require(scales)
plot2 <- ggplot() + geom_bar(aes(y = proportion, x = eventtype),data = costsub,stat="identity")
plot2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x="Event", y="Proportion of cost") + scale_y_continuous(labels = comma) + 
  geom_point(aes(y=proportion, x=eventtype), data=eventsub)
