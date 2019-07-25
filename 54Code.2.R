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
recent <- select(recent, STATE__, BGN_DATE,END_DATE, COUNTY, STATE, EVTYPE, FATALITIES, INJURIES )
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
recent <- select(recent, STATE__, BGN_DATE,END_DATE, COUNTY, STATE, FATALITIES, INJURIES, eventtype )
recent <- mutate(recent, eventtype = toupper(trimws(eventtype)))
recent <- mutate(recent, eventtype=as.factor(eventtype))

#Narrow down to key death events

dataevents <- group_by(recent, eventtype)
head(dataevents)
summary(dataevents)
deathrisk <- summarise(dataevents, meandeath =mean(FATALITIES),
                       totaldeath=sum(FATALITIES))
#Only keep those where neither is 0
deathrisk <- mutate(deathrisk, totalflag = "")
for (i in (1:nrow(deathrisk))){
        if(sum(deathrisk[i,2:3])==0){
                deathrisk[i,4] <- 0} else{
                        deathrisk[i,4] <- 1
        }
}
deathrisk<- deathrisk[deathrisk$totalflag=="1",]
deathrisk

meandeathrisk <- arrange(deathrisk, desc(meandeath))
totaldeathrisk<- arrange(deathrisk, desc(totaldeath))
meandeathrisk <- meandeathrisk[1:10,1]
totaldeathrisk <-totaldeathrisk[1:10,1]

#combine and get unique
allrisks <- c(meandeathrisk, totaldeathrisk)
vect <- unlist(allrisks)
interest <- (unique(vect))
sort(interest)

#Then filter table, and visualise
interesttable <- deathrisk[deathrisk$eventtype %in% interest,]
interesttable
interesttable$eventtype <- droplevels(interesttable$eventtype)
str(interesttable$eventtype)
interesttable

#Orderlevels
levorder <- interesttable$eventtype[order(interesttable$totaldeath, decreasing = TRUE)]
interesttable$eventtype <- factor(interesttable$eventtype, levels=levorder)
interesttable

#Narrow down to key injury

injrisk <- summarise(dataevents, meaninj =mean(INJURIES),
                       totalinj=sum(INJURIES))
#Only keep those where neither is 0
injrisk <- mutate(injrisk, totalflag = "")
for (i in (1:nrow(injrisk))){
        if(sum(injrisk[i,2:3])==0){
                injrisk[i,4] <- 0} else{
                        injrisk[i,4] <- 1
                }
}
injrisk<- injrisk[injrisk$totalflag=="1",]
injrisk

meaninkrisk <- arrange(injrisk, desc(meaninj))
totalinjrisk<- arrange(injrisk, desc(totalinj))
meaninkrisk <- meaninkrisk[1:10,1]
totalinjrisk <-totalinjrisk[1:10,1]

#combine and get unique
allrisksinj <- c(meaninkrisk, totalinjrisk)
vectinj <- unlist(allrisksinj)
interestinj <- (unique(vectinj))
sort(interestinj)

#Then filter table, and visualise
interesttableinj <- injrisk[injrisk$eventtype %in% interestinj,]
interesttableinj
interesttableinj$eventtype <- droplevels(interesttableinj$eventtype)
str(interesttableinj$eventtype)
interesttableinj

#Orderlevels
levorder2 <- interesttableinj$eventtype[order(interesttableinj$totalinj, decreasing = TRUE)]
interesttableinj$eventtype <- factor(interesttableinj$eventtype, levels=levorder2)
interesttableinj

#USe mean daeth and total death
par(mar = c(9,5,2,6))
with(interesttable, plot.default(eventtype, totaldeath, type="p", pch=16, col="blue", axes=FALSE, ylab="Total deaths 1996-2011", xlab=""))
axis(side = 1, at = as.numeric(interesttable$eventtype), labels = interesttable$eventtype, ylim=2000, las=2, xlab="")
axis(side=2)
par(new = T)
with(interesttable, plot.default(eventtype, meandeath, type="p", axes=F, xlab=NA, ylab=NA, cex=1.2, pch=4))
axis(side=4)
mtext(side = 4, line = 3, 'Mean deaths 1996-2011')
box()
legend("topright",
       legend=c("Total deaths", "Mean deaths"),
       pch=c(16, 4), col=c("blue", "black"))


#USe mean injury and total injury
par(mar = c(9,5,2,6))
with(interesttableinj, plot.default(eventtype, totalinj, type="p", pch=16, col="blue", axes=FALSE, ylab="Total injuries 1996-2011", xlab=""))
axis(side = 1, at = as.numeric(interesttableinj$eventtype), labels = interesttableinj$eventtype, ylim=2000, las=2, xlab="")
axis(side=2)
par(new = T)
with(interesttableinj, plot.default(eventtype, meaninj, type="p", axes=F, xlab=NA, ylab=NA, cex=1.2, pch=4))
axis(side=4)
mtext(side = 4, line = 3, 'Mean injuries 1996-2011')
box()
legend("topright",
       legend=c("Total injuries", "Mean injuries"),
      pch=c(16, 4), col=c("blue", "black"))


