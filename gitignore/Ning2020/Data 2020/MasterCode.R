#### Merging individual data frames ####

is.data.frame(Meta) # checking object is df -> TRUE
is.data.frame(Use) # checking object is df -> TRUE
is.data.frame(Avidity) # checking object is df -> TRUE

MetaUse<- merge(Meta, Use, by.x = "GlobalID", 
                   by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge Meta and Use dfs by GlobalID and GUID column
MasterNing<- merge(MetaUse, Avidity, by.x = "GlobalID.y", 
                by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge MetaUse and Avidity dfs by GlobalUse and GUIDavid column

write.csv(MasterNing, 'MasterNing.csv') # Need to fine tune the data on excel so saving as CSV in current workinmg directory. 

## Merging SurveyType, SurveyType_two, Shift of MasterNing to Meta

names(MasterNing) ## Checking names of variables

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

DF <- MasterNing %>%
  select(GlobalID.x, SurveyType, SurveyType_two, Shift) ##Subsetting using dplyr

DF

MetaED<-merge(Meta, DF, by.x='GlobalID', by.y='GlobalID.x')

write.csv(MetaED, 'MetaED.csv')
