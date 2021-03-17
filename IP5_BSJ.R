library(tidyverse)
library(stringi)
library(psych)
p<-read.csv("airlineDelays.csv")

p<-subset(p, select=-c(YEAR,MONTH,DAY_OF_MONTH,OP_CARRIER_FL_NUM,DEP_DELAY_NEW,ARR_DELAY_NEW,X))
#cleans unusable data from the set

p<-subset(p, p$CANCELLED==0)
p<-subset(p, p$DIVERTED==0)
#removes flights that were cancelled or diverted

p<-subset(p, select=-c(CANCELLED,DIVERTED))
#removes now useless columns

p$DAY_OF_WEEK[p$DAY_OF_WEEK==1]<-"Monday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==2]<-"Tuesday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==3]<-"Wednesday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==4]<-"Thursday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==5]<-"Friday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==6]<-"Saturday"
p$DAY_OF_WEEK[p$DAY_OF_WEEK==7]<-"Sunday"
#renames numerical data to days of the week

p$FL_DATE<-as.Date(p$FL_DATE)
#changes type to Date type

p$CARRIER_DELAY<-replace_na(p$CARRIER_DELAY,0)
p$WEATHER_DELAY<-replace_na(p$WEATHER_DELAY,0)
p$NAS_DELAY<-replace_na(p$NAS_DELAY,0)
p$SECURITY_DELAY<-replace_na(p$SECURITY_DELAY,0)
p$LATE_AIRCRAFT_DELAY<-replace_na(p$LATE_AIRCRAFT_DELAY,0)
#replaces NA values with 0, as if there was no delay caused by any of these conditions (NA in the set), we count that as 0

p$DEP_TIME<-as.character(p$DEP_TIME)
p$WHEELS_OFF<-as.character(p$WHEELS_OFF)
p$WHEELS_ON<-as.character(p$WHEELS_ON)
p$ARR_TIME<-as.character(p$ARR_TIME)
#prepares data in these rows to be manipulated correctly

p$DEP_TIME<-stri_sub_replace(p$DEP_TIME,-2,-1,value=paste(":",stri_sub(p$DEP_TIME,-2,-1),sep=""))
p$WHEELS_OFF<-stri_sub_replace(p$WHEELS_OFF,-2,-1,value=paste(":",stri_sub(p$WHEELS_OFF,-2,-1),sep=""))
p$WHEELS_ON<-stri_sub_replace(p$WHEELS_ON,-2,-1,value=paste(":",stri_sub(p$WHEELS_ON,-2,-1),sep=""))
p$ARR_TIME<-stri_sub_replace(p$ARR_TIME,-2,-1,value=paste(":",stri_sub(p$ARR_TIME,-2,-1),sep=""))
#times now read "##:##"

temp <-p %>% count(DAY_OF_WEEK)
barplot(temp$n,names.arg = temp$DAY_OF_WEEK)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'dayOfWeekFrequency.csv',row.names=F)
#First, we create a dataframe with two columns that represent the individual values and the frequency of them in the set
#Then, we create a bar plot of that data, which was used later on in the analysis, but is best to create here
#then we generate the proportion column, and rename the frequency column appropriately
#finally we write these tables to .csv files.

#every subsequent block from here until the next column follows the same structure as the block above.
#the fisrt line here, though, recreates the temp dataframe.
temp <-p %>% count(FL_DATE)
barplot(temp$n,names.arg = temp$FL_DATE)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'flightDateFrequency.csv',row.names=F)

temp <-p %>% count(ORIGIN)
barplot(temp$n,names.arg = temp$ORIGIN)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'originFrequency.csv',row.names=F)

temp <-p %>% count(ORIGIN_CITY_NAME)
barplot(temp$n,names.arg = temp$ORIGIN_CITY_NAME)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'originCityFrequency.csv',row.names=F)

temp <-p %>% count(ORIGIN_STATE_ABR)
barplot(temp$n,names.arg = temp$ORIGIN_STATE_ABR)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'originStateFrequency.csv',row.names=F)

temp <-p %>% count(DEST)
barplot(temp$n,names.arg = temp$DEST)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'destinationFrequency.csv',row.names=F)

temp <-p %>% count(DEST_CITY_NAME)
barplot(temp$n,names.arg = temp$DEST_CITY_NAME)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'destinationCityFrequency.csv',row.names=F)

temp <-p %>% count(DEST_STATE_ABR)
barplot(temp$n,names.arg = temp$DEST_STATE_ABR)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'destinationStateFrequency.csv',row.names=F)

temp <-p %>% count(DEP_TIME)
barplot(temp$n,names.arg = temp$DEP_TIME)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'departureTimeFrequency.csv',row.names=F)

temp <-p %>% count(WHEELS_OFF)
barplot(temp$n,names.arg = temp$WHEELS_OFF)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'wheelsOffFrequency.csv',row.names=F)

temp <-p %>% count(WHEELS_ON)
barplot(temp$n,names.arg = temp$WHEELS_ON)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'wheelsOnFrequency.csv',row.names=F)

temp <-p %>% count(ARR_TIME)
barplot(temp$n,names.arg = temp$ARR_TIME)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'arrivalTimeFrequency.csv',row.names=F)

temp <-p %>% count(DEP_DELAY_GROUP)
barplot(temp$n,names.arg = temp$DEP_DELAY_GROUP)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'departureDelayGroupFrequency.csv',row.names=F)

temp <-p %>% count(ARR_DELAY_GROUP)
barplot(temp$n,names.arg = temp$ARR_DELAY_GROUP)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'arrivalDelayGroupFrequency.csv',row.names=F)

temp <-p %>% count(DISTANCE_GROUP)
barplot(temp$n,names.arg = temp$DISTANCE_GROUP)
temp$Proportion<-temp$n/366940
names(temp)[2]<-"Frequency"
write.csv(temp,'distanceGroupFrequency.csv',row.names=F)


subCont<-subset(p,select=c(DEP_DELAY,ARR_DELAY,ACTUAL_ELAPSED_TIME,AIR_TIME,DISTANCE,CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY))
subDisc<-subset(p,select=-c(DEP_DELAY,ARR_DELAY,ACTUAL_ELAPSED_TIME,AIR_TIME,DISTANCE,CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY))
#These subsets of our variable list will be used to create appropriate visualizations from here on

temp2<-describe(subCont)
write.csv(temp2,'continuousVariableDescriptiveStatistics.csv')
#This table gives summary statistics for all continuous variables

temp2<-cor(subCont)
write.csv(temp2,'continuousVariablesCorrelationMatrix.csv')
hmCorrMat<-heatmap(temp2)
#creates a correlation matrix and displays it as a heatmap

depDelayH<-hist(p$DEP_DELAY)
arrDelayH<-hist(p$ARR_DELAY)
actTimeElapsedH<-hist(p$ACTUAL_ELAPSED_TIME)
airTimeH<-hist(p$AIR_TIME)
distH<-hist(p$DISTANCE)
carrDelayH<-hist(p$CARRIER_DELAY)
weaDelayH<-hist(p$WEATHER_DELAY)
secDelayH<-hist(p$SECURITY_DELAY)
nasDelayH<-hist(p$NAS_DELAY)
lateCraftDelayH<-hist(p$LATE_AIRCRAFT_DELAY)
#creates histograms for all continuous variables

pairs(subCont[,1:2],pch=19,lower.panel = NULL)
#Just this one scatterplot alone takes about half a minute on my machine to load, likely due to the sheer volume of data.
#As such, the following command will in fact create a pairwise plot of all the continuous variables
#But as it is creating 54 plots, I would not recommend running it
pairs(subCont,pch=19,lower.panel = NULL)

