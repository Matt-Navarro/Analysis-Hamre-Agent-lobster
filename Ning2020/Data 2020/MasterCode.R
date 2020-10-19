#### Merging individual data frames ####

is.data.frame(Meta) # checking object is df -> TRUE
is.data.frame(Use) # checking object is df -> TRUE
is.data.frame(Avidity) # checking object is df -> TRUE


# To merge I need to connect:
# Global ID of Meta --> GUID of USE
# GlobalID of Use --> GUID of avidity

# However have the same column names is going to confuse things so i'm going to change GlobalID of Use to GlobalUse and the GUID of avid to GUIDavidity
# Therefore i'll be connecting:
#GlobalID (Meta) --> GUID (Use)
#GlobalUse (Use) --> GUIDavidity (avidity)

## installing tidyverse library with dplyr
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

Use %>% rename(GlobalUse = GlobalID) # Changing GlobalID to GlobalUse in Use df
Avidity %>% rename(GUIDavid = Guid) # Changing GUID to GUIDavid in avidity df

# The above section on changing the column names was actually unnessecesary
# R changed the names of thecoumns with the same name automatically, but worth knowing how to reanme stuff. 


MetaUse<- merge(Meta, Use, by.x = "GlobalID", 
                   by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge Meta and Use dfs by GlobalID and GUID column
MasterNing<- merge(MetaUse, Avidity, by.x = "GlobalID.y", 
                by.y = "Guid", all.x = TRUE, all.y = TRUE) # merge MetaUse and Avidity dfs by GlobalUse and GUIDavid column

write.csv(MasterNing, 'MasterNing.csv') # Need to fine tune the data on excel so saving as CSV
