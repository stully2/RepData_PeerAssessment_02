filename <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
temp <- tempfile()
download.file(filename,temp,mode="wb")
mydata <- read.csv(temp)
unlink(temp)
rm(list = c("filename","temp"))
library(plyr)
library(dplyr)

mydata2 <- ddply(mydata
                  ,.(COUNTYNAME,STATE,EVTYPE,PROPDMGEXP,CROPDMGEXP)
                  , summarize
                  ,fatalities = sum(FATALITIES,na.rm = TRUE)
                  ,injuries = sum(INJURIES,na.rm = TRUE)
                  ,propdamage = sum(PROPDMG,na.rm = TRUE)
                  ,cropdamage = sum(CROPDMG,na.rm = TRUE)
                 )

conv <- data.frame(code = c("0","1","2","3","4","5","6","7","8","-","?","+","B","h","K","m","b","H","k","M")
                  ,mult = c(0,10,100,1000,10000,100000,1000000,10000000,100000000,0,0,1,1000000000,100,1000,1000000,1000000000,100,1000,1000000)
)

mydata3 <- merge(mydata2,conv,by.x = c("PROPDMGEXP"),by.y = c("code"), x.all = TRUE)
mydata3 <- merge(mydata3,conv,by.x = c("CROPDMGEXP"),by.y = c("code"), x.all = TRUE)
mydata3 <- mutate(mydata3,mod_propdamg = propdamage * mult.x,mod_cropdamg = cropdamage * mult.y)
mydata3 <- select(mydata3,EVTYPE,PROPDMGEXP,COUNTYNAME,STATE,CROPDMGEXP,cropdamage,fatalities,injuries,mod_propdamg,mod_cropdamg)

q1data <- ddply(
            mydata3
            , .(EVTYPE), summarize
            ,fatalities = sum(fatalities,na.rm = TRUE)
            ,injuries = sum(injuries,na.rm = TRUE)
            ,totals =  sum(fatalities,na.rm = TRUE)+sum(injuries,na.rm = TRUE)
            )





q2data <- ddply(
  mydata3
  , .(EVTYPE), summarize
  ,propertydamage = round(sum(mod_propdamg,na.rm = TRUE)/1000000,0)
  ,cropdamage = round(sum(mod_cropdamg,na.rm = TRUE)/1000000,0)
  ,totals = round(sum(mod_propdamg + mod_cropdamg)/1000000,0)
  )