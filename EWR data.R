
cat("\014") # clear the console
rm(list=ls())
dev.off() 

library(reshape2) 
library(stringr) 
library(plyr)
library(dplyr) 
library(formattable) 
library(doBy) 
library(ggplot2) 

#setwd("~/Dropbox/Work and research/Port Authority/PA data & analysis/PA aviation user") #home
setwd("C:/Users/ceshleman/Dropbox/Work and research/Port Authority/PA data & analysis/PA aviation user") #work 

# Weekday 
a1 = read.csv("./EWR_FD_150914_150916.csv", skip = 7)
b1 = read.csv("./EWR_FD_160914_160916.csv", skip = 7) 
ab1 = rbind(a1,b1) 
  names(ab1) = tolower(names(ab1)) 
ab1$runway.occupancy.time=NULL
ab1$taxiway.used.to.enter.exit.runway=NULL 
ab1$daytype = "weekday" 

# Weekend 
list.files() 
a = read.csv("./EWR_FD_2016_WKND.csv") #read.csv("./EWR_FD_150914_150916.csv", skip = 7)
head(a) 
b = read.csv("./EWR_FD_2015_WKND.csv") #read.csv("./EWR_FD_160914_160916.csv", skip = 7) 
ab = rbind(a,b) 
names(ab) = tolower(names(ab))
ab$daytype = "weekend" 
rm(a,b) 

names(ab1) = names(ab) 
ab = rbind(ab1,ab) 


# prep prep 
ab = subset(ab, ab$operation=="Arrival" | ab$operation == "Departure") 
ab$day.time = strptime(ab$event.time, "%m/%d/%y %H:%M", tz = "EST5EDT") #strptime(ab$event.time..est., "%m/%d/%y %H:%M", tz = "EST5EDT") 
 
ab$date = format(as.POSIXct(ab$day.time,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
ab$date = as.Date(ab$date, format="%m/%d/%Y") 

ab$weekday = weekdays(ab$date) 

ab$time = as.POSIXct(as.numeric(as.POSIXct(ab$day.time)) %% 86400, origin = "2000-01-01") 
ab$time2 = as.numeric(as.POSIXct(ab$day.time)) %% 86400  # 60*60*24
benchpoints = c("2001-01-01 05:30:00", "2001-01-01 06:30:00", "2001-01-01 9:30:00", 
                "2001-01-01 16:30:00", "2001-01-01 19:30:00", "2001-01-01 23:00:00") 
benchpoints2 = as.numeric(as.POSIXct(benchpoints)) %% 86400 

# Effective hours are everything but 11pm-6am. 86400 seconds in a day. 
str(ab$time) 
ab$timecat = ifelse(ab$time2>=82800 | ab$time2<=21600,"Night","SixAMtoElevenPM") #18000
    
ab = droplevels(ab) 
table(ab$timecat) 
table(ab$timecat,ab$operation) 
head(ab) 
ab_wknd = subset(ab,ab$daytype=="weekend") 
ab_wkdy = subset(ab,ab$daytype=="weekday") 
ab_wknd = table(ab_wknd$timecat,ab_wknd$runway) 
ab_wkdy = table(ab_wkdy$timecat,ab_wkdy$runway) 
setwd("C:/Users/ceshleman/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/CBA LCCA CEA TCO/LCCA projects/LCCA EWR 4R 2019")
#setwd("~/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/CBA LCCA CEA TCO/LCCA projects/LCCA EWR 4R 2019")
#write.csv(ab_wknd,"./Time of day weekend 20190503.csv") 
#write.csv(ab_wkdy,"./Time of day weekday 20190503.csv") 
write.csv(ab,"./All runway data 20190503.csv")

head(ab) 
table(ab$daytype, ab$operation) 

# Basic density
p = ggplot(ab, aes(x=time2)) + 
  geom_density()
p
# Add mean line
p + geom_vline(aes(xintercept=mean(time2)),
              color="blue", linetype="dashed", size=1)

getwd() 

options(scipen=5)
pq = ggplot(ab, aes(x=time2, fill=daytype)) +
  geom_density(alpha=0.2) + labs(x = "", y = "") 
pq 
ggsave(pq, file="./Activity_EWR.png", height=4,width=6) 
mean(ab$time2) 
#ggsave(p,file="./PA PATH/PA PATH output & viz/C. Monthly weekday.png", width=8,height=5) 

#ampeak = subset(ab,ab$timecat=="ampeak") 
#pmpeak = subset(ab,ab$timecat=="pmpeak") 
#night = subset(ab,ab$timecat=="night") 
#offpeak = subset(ab,ab$timecat=="offpeak") 
#table(ampeak$runway) 

# actual.landing.time..aerobahn...est. 

### 
