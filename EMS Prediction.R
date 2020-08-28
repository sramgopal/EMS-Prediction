##Load required packages

library(zoo)
library(lubridate)
library(data.table)
library(plyr)
library(dplyr)
require(MASS)
require(magrittr)
library(nnet)
library(splitstackshape)
library(NeuralNetTools)
library(caret)
library(SuperLearner)
library(ggplot2)
library(arm)
library(glmnet)
library(gam)
library(ranger)
library(xgboost)
library(earth)
library(cvAUC)
library(MASS)

#Load the EMS data. This can be obtained online at https://data.cityofnewyork.us/Public-Safety/EMS-Incident-Dispatch-Data/76xm-jjuj
#Note: at the time of this writing, the data exceeded 4GB

limitedEMS = fread("EMS_Incident_Dispatch_Data.csv", select = c("CAD_INCIDENT_ID", "INCIDENT_DATETIME",
                                                                                                              "INITIAL_CALL_TYPE", "INITIAL_SEVERITY_LEVEL_CODE","FINAL_CALL_TYPE","FINAL_SEVERITY_LEVEL_CODE","FIRST_ACTIVATION_DATETIME","BOROUGH","ZIPCODE","TRANSFER_INDICATOR","STANDBY_INDICATOR","SPECIAL_EVENT_INDICATOR"), header=TRUE,showProgress = TRUE)

limitedEMS$date2 <- parse_date_time(limitedEMS$INCIDENT_DATETIME, '$m/%d/%Y %I:%M:%S %p')
limitedEMS$date3 <- parse_date_time(limitedEMS$FIRST_ACTIVATION_DATETIME, '$m/%d/%Y %I:%M:%S %p')

limitedEMS <- subset(limitedEMS,!is.na(date3) )
date1 <- as.POSIXct("2014-12-31 01:50:00", tz="GMT")
date2 <- as.POSIXct("2020-01-01 23:53:00", tz="GMT")
int <- interval(date1, date2)

limitedEMS <- limitedEMS[limitedEMS$date3 %within% int,]
rm(date1,date2,int)

# Weather data can be downloaded at https://www.noaa.gov/
# A sample dataset is provided on the github page.
Weather <- read.csv("2160929.csv")
Suntimes <- Weather
Weather$DATE <- as.POSIXct(Weather$DATE,format="%Y-%m-%dT%H:%M", tz="GMT")
Weather<-Weather[!(as.numeric(format(Weather$DATE, "%M"))!=51),]
Weather <- Weather[rev(order(Weather$DATE)),]
Weather$DATE_next <- lag(Weather$DATE, n=1)
Weather <- Weather[order(Weather$DATE),]
Weather$timediff <- difftime(Weather$DATE_next, Weather$DATE, units="min")

#now clean up the weather data.
Weather$HourlyPrecipitation <- as.numeric(as.character(Weather$HourlyPrecipitation))
Weather$HourlyPrecipitation[is.na(Weather$HourlyPrecipitation)] <- 0
Weather$rain <-0
Weather$snow <-0
Weather$rain[grepl("TS",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("RA",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("SH",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("DZ",Weather$HourlyPresentWeatherType,ignore.case=TRUE) ] <- 1 # RA rain, TS thunderstorm SH showers DZ drizzle
Weather$snow[grepl("FZ",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("SG",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("SN",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("IC",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("PL",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("GR",Weather$HourlyPresentWeatherType,ignore.case=TRUE) | grepl("GS",Weather$HourlyPresentWeatherType,ignore.case=TRUE) ] <- 1 # GS, GR, PL, IC, SN, SG,FZ
EMSdata <- data.frame(Weather$DATE,Weather$DATE)
names(EMSdata)[names(EMSdata) == 'Weather.DATE'] <- 'starthr'
Weather$temp <- (as.numeric(as.character(Weather$HourlyDryBulbTemperature)) - 32)/1.8 #convert temp F to c
Weather$dewpt <- (as.numeric(as.character(Weather$HourlyDewPointTemperature)) - 32)/1.8 #convert temp F to c
Weather$HourlyWindSpeed <- as.numeric(as.character(Weather$HourlyWindSpeed))
Weather$HourlyVisibility <- as.numeric(as.character(Weather$HourlyVisibility))
Weather$HourlyPrecipitation <- as.numeric(as.character(Weather$HourlyPrecipitation))
Weather$humid <- as.numeric(as.character(Weather$HourlyRelativeHumidity))
Weather$pressure <- as.numeric(as.character(Weather$HourlySeaLevelPressure))
Weather$pressure <- Weather$pressure*25.40
Weather$HourlyVisibility <- Weather$HourlyVisibility * 1609.344 #convert visibility to mters
Weather$HourlyPrecipitation <- Weather$HourlyPrecipitation * 25.4 #convert precip to millimeters
Weather$HourlyWindSpeed <- Weather$HourlyWindSpeed * 0.4470389 #convert windspeed to meters/second

Weather2 <- subset(Weather,select=c(DATE,temp,HourlyVisibility,dewpt,humid,HourlyWindSpeed,HourlyPrecipitation,pressure))
vis <- data.frame(rollapply(Weather2[, 3], 6, mean, na.rm=TRUE))
dewpt <- data.frame(rollapply(Weather2[, 4], 6, mean, na.rm=TRUE))
humid <- data.frame(rollapply(Weather2[, 5], 6, mean, na.rm=TRUE))
ws <- data.frame(rollapply(Weather2[, 6], 6, mean, na.rm=TRUE))
precip <- data.frame(rollapply(Weather2[, 7], 6, sum, na.rm=TRUE))
pressure <- data.frame(rollapply(Weather2[, 8], 6, mean, na.rm=TRUE))

EMSdata$rain <- rollsumr(Weather$rain, k = 6, fill = NA)
EMSdata$rain[EMSdata$rain>=1] <-1
EMSdata$snow <- rollsumr(Weather$snow, k = 6, fill = NA)
EMSdata$snow[EMSdata$snow>=1] <-1
EMSdata$anyprecip<-0
EMSdata$anyprecip[EMSdata$snow==1 | EMSdata$rain ==1] <-1
EMSdata$dayofweek <- as.POSIXlt(EMSdata$starthr)$wday
EMSdata$weekend<-0
EMSdata$dayofweek <- factor(EMSdata$dayofweek,levels=c(0,1,2,3,4,5,6), labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
EMSdata$weekend[EMSdata$dayofweek == "Saturday" | EMSdata$dayofweek == "Sunday"] <-1

EMSdata$month = as.numeric(format(EMSdata$starthr, "%m"))
EMSdata$season[EMSdata$month==1 | EMSdata$month==2 | EMSdata$month==3] <- 1
EMSdata$season[EMSdata$month==4 | EMSdata$month==5 | EMSdata$month==6] <- 2
EMSdata$season[EMSdata$month==7 | EMSdata$month==8 | EMSdata$month==9] <- 3
EMSdata$season[EMSdata$month==10 | EMSdata$month==11 | EMSdata$month==12] <- 4
EMSdata$season <- factor(EMSdata$season,levels=c(1,2,3,4), labels=c("Winter","Spring","Summer","Fall"))

EMSdata <- EMSdata[-c(1:5), ]
EMSdata$temp <- temp$rollapply.Weather2...2...6..mean..na.rm...TRUE.
EMSdata$vis <- vis$rollapply.Weather2...3...6..mean..na.rm...TRUE.
EMSdata$dewpt <- dewpt$rollapply.Weather2...4...6..mean..na.rm...TRUE.
EMSdata$humid <- humid$rollapply.Weather2...5...6..mean..na.rm...TRUE.
EMSdata$precip <- precip$rollapply.Weather2...7...6..sum..na.rm...TRUE.
EMSdata$wind <- ws$rollapply.Weather2...6...6..mean..na.rm...TRUE.
EMSdata$pressure <- pressure$rollapply.Weather2...8...6..mean..na.rm...TRUE.


EMSdata$Weather.HOURLYDRYBULBTEMPC<-NULL
EMSdata$Weather.HourlyVisibility<-NULL
EMSdata$Weather.HOURLYDewPointTempC<-NULL
EMSdata$Weather.HOURLYRelativeHumidity<-NULL
EMSdata$Weather.HOURLYWindSpeed<-NULL
EMSdata$Weather.HourlyPrecipitation<-NULL

t.lub <- ymd_hms(EMSdata$starthr)
h.lub <- hour(t.lub) + minute(t.lub)/60

EMSdata$time <- h.lub
rm(h.lub)
rm(t.lub)
EMSdata$time <-as.integer(EMSdata$time)


Suntimes$DATE <- as.POSIXct(Suntimes$DATE,format="%Y-%m-%dT%H:%M", tz="GMT")
Suntimes$nchar <- as.character(nchar(Suntimes$Sunrise))
Suntimes$Sunrise <- paste(0,as.character(Suntimes$Sunrise))
Suntimes$Sunrise <- gsub(" ", "", Suntimes$Sunrise, fixed = TRUE)
Suntimes$sunrisetime <- paste(as.character(as.Date(Suntimes$DATE)),Suntimes$Sunrise)
Suntimes$sunset <- paste(as.character(as.Date(Suntimes$DATE)),Suntimes$Sunset)

Suntimes$sunrisetime1 <-  as.POSIXct(Suntimes$sunrisetime, format="%Y-%m-%d %H%M", tz="GMT")
Suntimes$sunsettime <-  as.POSIXct(Suntimes$sunset, format="%Y-%m-%d %H%M", tz="GMT")
Suntimes$date_only <-  as.Date(Suntimes$sunset)
Suntimes <- subset(Suntimes,!is.na(Suntimes$sunrisetime1))
Suntimes$daytime <- interval(Suntimes$sunrisetime1,Suntimes$sunsettime)

EMSdata$date_only <- as.Date(EMSdata$starthr)
EMSdata <- merge(x = EMSdata, y = subset(Suntimes,select=c(date_only,daytime)), by = "date_only", all.x = TRUE)
EMSdata$daynight <- EMSdata$starthr %within% EMSdata$daytime # TRUE
EMSdata$timeofday[EMSdata$daynight==TRUE] <- 1
EMSdata$timeofday[is.na(EMSdata$timeofday)] <- 0
EMSdata$daynight <- NULL
EMSdata$daytime <- NULL


##The following code will allocate the dispatch times into hourly windows
EMSdata$window <- EMSdata$starthr-59*60-59
EMStimes <- data.frame(limitedEMS$date3)
EMStimes$Frequency <-1
dispatchtracker <- data.frame(EMStimes$limitedEMS.date3, EMStimes$Frequency)
mydata <- data.frame(EMSdata$window, EMSdata$starthr)
names(mydata)[names(mydata) == 'EMSdata.window'] <- 'DateTime1'
names(mydata)[names(mydata) == 'EMSdata.starthr'] <- 'DateTime2'
names(dispatchtracker)[names(dispatchtracker) == 'EMStimes.limitedEMS.date3'] <- 'DateTime'
names(dispatchtracker)[names(dispatchtracker) == 'EMStimes.Frequency'] <- 'Frequency'
mydata$Interval <- as.interval(mydata$DateTime1, mydata$DateTime2)
mydata$SumFrequency <- NA
mydata$NumInt <- 1:nrow(mydata)
mydata$SumFrequency <- dlply(mydata, .(NumInt),
                             function(row){
                               sum(
                                 dispatchtracker[dispatchtracker$DateTime %within% row$Interval, "Frequency"]
                               )
                             }, .progress='text')

EMSdata$counts <- mydata$SumFrequency
EMSdata$counts <- as.integer(EMSdata$counts) 

sum(EMSdata$counts)
rm(dispatchtracker)
rm(EMStimes)
names(EMSdata)[names(EMSdata) == 'counts'] <- 'NYCDispatch'
runs <- subset(EMSdata,select=c(starthr,NYCDispatch))
runs <- c(NA,NA,NA,NA,NA,NA,rollapply(runs[, 2], 6, mean, na.rm=TRUE))
n<-dim(EMSdata)[1]
EMSdata<-EMSdata[1:(n+1),]
EMSdata$prev6ave <- runs
n<-dim(EMSdata)[1]
EMSdata<-EMSdata[1:(n-1),]

date1 <- as.POSIXct("2015-01-01 01:50:00", tz="GMT")
date2 <- as.POSIXct("2019-12-31 23:53:00", tz="GMT")
int <- interval(date1, date2)
EMSdata <- EMSdata[EMSdata$starthr %within% int,]


EMSdata$timeofday <- factor(EMSdata$timeofday,levels=c(0,1), labels=c("Day","Night"))

EMSdata$time <- as.factor(EMSdata$time)
EMSdata$month <- as.factor(EMSdata$month)
EMSdata$pressure <- na.locf(EMSdata$pressure)
EMSdata$rain <- factor(EMSdata$rain, levels = c("No rain", "Rain"), ordered = TRUE)
EMSdata$snow <- factor(EMSdata$snow, levels = c("No snow", "Snow"), ordered = TRUE)
EMSdata$dayofweek <- factor(EMSdata$dayofweek, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), ordered = TRUE)
EMSdata$month <- factor(EMSdata$month, levels = c(1:12), ordered = TRUE)
EMSdata$time <- factor(EMSdata$time, levels = c(0:23), ordered = TRUE)
EMSdata$timeofday <- factor(EMSdata$timeofday, levels = c("Day", "Night"), ordered = TRUE)
EMSdata$year <- as.numeric(year(EMSdata$date_only))

##Split into cohorts.

sample_size = floor(0.75*nrow(EMSdata))
summary(EMSdata$time)

EMSdata_refined <- subset(EMSdata,select=c(rain,snow,dayofweek,month,temp,vis,dewpt,wind,time,timeofday,pressure,prev6ave,year,NYCDispatch))
set.seed(108)
picked = sample(seq_len(nrow(EMSdata_refined)),size = sample_size)
development =EMSdata_refined[picked,]
holdout =EMSdata_refined[-picked,]
outcomes <- development$NYCDispatch
predictors <- development[1:13]

#cross validated superlearner
set.seed(108)
CV.sl.fit <- CV.SuperLearner(outcomes, predictors, family=gaussian(), V = 10, method="method.NNLS", verbose=TRUE, SL.library = c("SL.glm", "SL.gam",
                                                                                                                                 "SL.ranger", "SL.earth","SL.xgboost"))
summary(CV.sl.fit)

#non-crossvalidated superlearner to generate predictions
sl.fit <- SuperLearner(outcomes, predictors, family=gaussian(), method="method.NNLS", verbose=TRUE,
                       SL.library = c("SL.glm", "SL.gam","SL.ranger", "SL.earth","SL.xgboost"))



holdouts <- subset(holdout,select=c(1:13))
holdout$pred <- predict.SuperLearner(sl.fit, holdouts)$pred

#error assessments can be made on the holdout and development datasets.
development$pred <- predict.SuperLearner(sl.fit, development[1:13])$pred
Metrics::rmse(development$NYCDispatch,development$pred)
Metrics::mse(development$NYCDispatch,development$pred)
Metrics::mae(development$NYCDispatch,development$pred)

