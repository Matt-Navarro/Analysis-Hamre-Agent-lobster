##### Packages #####

install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
library(dplyr)

#### Merging individual data frames ####
# Merging the Meta, use and avidity surveys

is.data.frame(Meta) # checking object is df -> TRUE
is.data.frame(Use) # checking object is df -> TRUE
is.data.frame(Avidity) # checking object is df -> TRUE

MetaUse<- merge(Meta, Use, by.x = "GlobalID", 
                by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge Meta and Use dfs by GlobalID and GUID column
Ning1<- merge(MetaUse, Avidity, by.x = "GlobalID.y", 
                   by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge MetaUse and Avidity dfs by GlobalUse and GUIDavid column

write.csv(Ning1, 'Ning1.csv') # Need to fine tune the data on excel so saving as CSV in current workinmg directory. 

## Merging SurveyType, SurveyType_two, Shift of Ning1 to Meta

names(Ning1) ## Checking names of variables

DF <-Ning1 %>%
  select(GlobalID.x, SurveyType, SurveyType_two, Shift) ##Subsetting using dplyr

DF

MetaED<-merge(Meta, DF, by.x='GlobalID', by.y='GlobalID.x')

write.csv(MetaED, 'MetaED.csv')

## 11/11/20: Imported JonDat and NingDat

is.data.frame(NingDat) # checking object is df -> TRUE
names(NingDat)

[1] "ID"                  "WhosData"            "Date"                "InterviewTime"       "Interviewer"         "Transect"           
[7] "Access"              "TotDaysInArea"       "BoatAccess"          "DaysOnBoat"          "DaysOnShore"         "TimesInTwenty"      
[13] "TimesInNineteen"     "Covid"               "Meta.x"              "Meta.y"              "Use.x"               "Use.y"              
[19] "ActivityDate"        "FishingType"         "BaitLure"            "TimeStartEX"         "TimeEndEX"           "MedianTimeEX"       
[25] "DecMedianTimeEX"     "HoursEX"             "DecHourEX"           "MaxDepth"            "KeptUndam"           "ReleasedUndam"      
[31] "TotalUndam"          "DP"                  "TotalHooked"         "PerDP"               "ProDP"               "Species"            
[37] "WhyLeaveEx"          "Activity"            "TimeStartNEX"        "TimeEndNEX"          "MedianTimeNEX"       "DecMedianTimeNEX"   
[43] "HoursNEX"            "DecHourNEX"          "DivingDepth"         "WhyThisSite"         "WhyLeaveNEX"         "DaysOfYrFishedBoat" 
[49] "DaysOfYrFishedShore" "DaysOfYrFished"      "DaysOfYrNEX"         "YrsFishing"          "YrsNEX"              "DiveCert"           
[55] "DiveNum"             "YrBorn"              "Postcode"            "Accom"               "Sex"                 "Party"              
[61] "BoatLength"          "BoatType"            "BoatName"            "Comments"          

str(NingDat)

is.data.frame(JonDat) # checking object is df -> TRUE

# I want to use rbind so all the col names have to be the same 
names(NingDat) # get names for NingDat
names(JonDat) # Get names for JonDat 

JonDat%>%
  rename(TimeEndEX = TimeOutEx)

source(Add file path for fast merge in here)
MasterNing<-fastmerge(NingDat, JonDat)
names(MasterNing)
str(MasterNing)

#### Unecessary Rabbit holes that may one day be useful ####

#Comparing dfs
install.packages("arsenal", dependencies=TRUE)
library(arsenal)

comparedf(NingDat, JonDat) #comparedf analysis
summary(comparedf(NingDat, JonDat))# comparedf summary