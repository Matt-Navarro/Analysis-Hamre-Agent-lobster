#### Install Packages & load librarys ---------------------------------

install.packages('lunar', dependencies=TRUE) ## adding lunar data 
install.packages('dplyr', dependencies=TRUE) 
install.packages('dismo', dependencies=TRUE) 
install.packages('gbm', dependencies=TRUE) 
install.packages('lunar', dependencies=TRUE) 
install.packages("devtools", dependencies=TRUE)
devtools::install_github("JBjouffray/ggBRT")

library(lunar)
library(dplyr)
library(dismo)
library(gbm)
library(devtools)
library(ggBRT)

#### Adding lunar data ------------------------------------------------

dat<-read.csv("NingMaster.csv",stringsAsFactors =T)

#### formatting Date variable 
names(dat)
dat$Date              
levels(dat$Date)
str(dat$Date)
dat$Date<-as.Date(dat$Date, "%Y-%m-%d")
dat$Date<-format(as.Date(dat$Date, "%Y-%m-%d"), "%d-%m-%Y")
dat$Date

dat$lunar<-lunar.phase(dat$Date, name=TRUE)
dat$lunar

write.csv(dat, "dat.csv")

#### Loading Data ---------------------------------------------------

#clear memory/workspace
rm(list=ls()) 

## Checking data 
setwd("~/Documents/UWA PhD/Ningaloo/Data") ## set working directory
getwd() ## check it is in right directory
dat<-read.csv("dat.csv",stringsAsFactors =T)%>%
  #   select predictor variables
  dplyr::select(c(Month.Year, Boat_ramp, Lat, Long, fivekm, Kernel.density, SST, DisFromBRkm,
                  No.FishDP, TotalFishHooked, DecMedian_time, DecFishingHours, FishMeth, BurleyUsed, BaitType,Max_hook,
                  Boat_Length, lunar))

names(dat)
#### West - Demersal ---------------------------------------------------

## subset to demersal fishing
dat_dem<-subset(dat, FishMeth%in%c("Bottom-bashing (drifting)", "Bottom-bashing (anchored)"))
dat.west_dem <- subset(dat, Boat_ramp%in%c("Tantabiddi", "Coral Bay"))

summary(dat.west_dem) ## summary of data
head(dat.west_dem, 3) ## First 3 rows in each col
str(dat.west_dem) ## Check data types
names(dat.west_dem) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.1, 0.05, 0.01), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid ## check grid

## use gbm.step to find optimal trees with a range of lr and tc combinations 
## sink("dp.stepW_dem.txt")
for(i in 1:nrow(grid)) {
  dp.stepW_dem<-dismo::gbm.step(data=dat.west_dem,
                           gbm.x=c(1:9, 12, 13, 15:19), ## explanatory
                           gbm.y=c(10),## response
                           ##offset=dp.offset, ## adds offset 
                           ##fold.vector=dat$folds, 
                           lr=grid[i,"lr"], ## ref to grid 
                           tc=grid[i,"tc"], ## ref to grid 
                           step.size=5,
                           family="poisson",
                           bag.fraction=0.5)## distribution family
  grid[i, "res.dev"]<-dp.stepW_dem$self.statistics$mean.resid} ### store res.dev in grid 
##sink()

grid  ## view grid 

## n=250, res.dev=2.755, lr=0.05, tc=5, nt=415

## plotting the chosen gbm.step 

ggInfluence(dp.stepW_dem) ## better influence summary - used gbm.step 
ggPD(dp.stepW_dem,rug = T) ## plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.stepW_dem) ## plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.stepW_dem,index = T) ## shows table of interactions-only showing baittype?


# Boostrap the BRT 1000 times to build confidence intervals
dp.stepW_dem.prerun<- plot.gbm.4list(dp.stepW_dem)

dp.stepW_dem.boot <- gbm.bootstrap.functions(dp.stepW_dem, list.predictors=dp.stepW_dem.prerun, n.reps=100)

# Draw PDPs 
PDbootW_dem<-ggPD_boot(dp.stepW_dem, list.4.preds=dp.stepW_dem.prerun, 
                booted.preds=dp.stepW_dem.boot$function.preds, type.ci="ribbon", rug=T)

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.stepW_dem, predictor="Burley", list.4.preds=dp.stepW_dem.prerun, 
                       booted.preds=dp.stepE_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## Bait Type
bait_boot<-ggPD_boot(dp.stepW_dem, predictor="BaitType", list.4.preds=dp.stepW_dem.prerun, 
                     booted.preds=dp.stepW_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.stepW_dem, predictor="Month.Year", list.4.preds=dp.stepW_dem.prerun, 
                      booted.preds=dp.stepW_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.stepW_dem, predictor="Boat_ramp", list.4.preds=dp.stepW_dem.prerun, 
                   booted.preds=dp.stepW_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?


##### gbm code - might not need to use 

dp.gbmW_dem<-gbm(data=dat_dem,
                No.FishDP~offset(TotalFishHooked)+Month.Year+Boat_ramp+No.Boats+Lat+Long+fivekm+Kernel.density+SST+
                  DisFromBRkm+DecMedian_time+DecFishingHours+GearCost_DP+FishMeth+Burley+BaitType+Max_hook+Boat_Length, ## formula
                distribution = "poisson", ## distribution of response 
                n.trees=1050,  ## number of trees 
                interaction.depth=1, ## tree complexity
                shrinkage=0.1, ## learning rate 
                bag.fraction=0.75,  ## bag fraction
                verbose=TRUE) ## print table

sink("dp.gbmW_dem.summary.txt")
par(mar=c(4, 10, 1, 1))
summary(dp.gbmW_dem, las=1) ## show relative influence table and plot
sink()
pretty.gbm.tree(dp.gbmW_dem, i.tree=1) 

par(mfrow=c(1,1)) 
plot.gbm(dp.gbmW_dem, i.var=c(1:12)) ## doesnt like vector 
dismo::gbm.plot.fits(dp.gbmW_dem)

## manual version of what i attempted above - wont go into matrix
plot.gbm(dp.gbmW_dem, i.var=c(1),ylab="No. Fish DP") ## Month/year
plot.gbm(dp.gbmW_dem, i.var=c(2),ylab="No. Fish DP") ## BR
plot.gbm(dp.gbmW_dem, i.var=c(3),ylab="No. Fish DP") ## no.boats - as expected
plot.gbm(dp.gbmW_dem, i.var=c(4),ylab="No. Fish DP") ## Lat
plot.gbm(dp.gbmW_dem, i.var=c(5),ylab="No. Fish DP") ## Long
plot.gbm(dp.gbmW_dem, i.var=c(6),ylab="No. Fish DP") ## five km
plot.gbm(dp.gbmW_dem, i.var=c(7),ylab="No. Fish DP") ## kernel density - opposite to expectation
plot.gbm(dp.gbmW_dem, i.var=c(8),ylab="No. Fish DP") ## SST 
plot.gbm(dp.gbmW_dem, i.var=c(9),ylab="No. Fish DP") ## DisFromBRkm - as expected 
plot.gbm(dp.gbmW_dem, i.var=c(10),ylab="No. Fish DP") ## median time 
plot.gbm(dp.gbmW_dem, i.var=c(11),ylab="No. Fish DP") ## fishing hours - opposite to expectation
plot.gbm(dp.gbmW_dem, i.var=c(12),ylab="No. Fish DP") ## gear cost - as expected 
plot.gbm(dp.gbmW_dem, i.var=c(13),ylab="No. Fish DP") ## FishMeth 
plot.gbm(dp.gbmW_dem, i.var=c(14),ylab="No. Fish DP") ## burley
plot.gbm(dp.gbmW_dem, i.var=c(15),ylab="No. Fish DP") ## bait
plot.gbm(dp.gbmW_dem, i.var=c(16),ylab="No. Fish DP") ## omax hook - posite to expectation
plot.gbm(dp.gbmW_dem, i.var=c(17),ylab="No. Fish DP") ### boat length -opposite to expectation

## Interactions
## code to find if there are any interactions in data set
dp.gbmW_dem.inter <- dismo::gbm.interactions(dp.gbmW_dem) ##undefined cols selcted - no col argument? 

## seeing strnegth of interactions
dp.gbmW_dem.inter<-interact.gbm(dp.gbmW_dem, data=dat.west_dem, i.var =(1:12)) # interaction depth too low in model call

#### East-Demersal ---------------------------------------------------

## subset to demersal fishing
dat_dem<- subset(dat, FishMeth%in%c("Bottom-bashing (drifting)", "Bottom-bashing (anchored)"))
dat.east_dem <- subset(dat, Boat_ramp%in%c("Bundegi", "Exmouth marina"))

summary(dat.east_dem ) ## summary of data
head(dat.east_dem , 3) ## First 3 rows in each col
str(dat.east_dem ) ## Chech data types
names(dat.east_dem ) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.001, 0.05, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid ## check grid

## use gbm.step to find optimal trees with a range of lr and tc combinations 
for(i in 1:nrow(grid)) {
  dp.stepE_dem<-dismo::gbm.step(data=dat.east_dem,
                               gbm.x=c(1:9, 12, 13, 15:19), ## explanatory
                               gbm.y=c(10),## response
                               lr=grid[i,"lr"], ## ref to grid 
                               tc=grid[i,"tc"], ## ref to grid 
                               family="poisson", ## distribution family
                               step.size=5) 
  grid[i, "res.dev"]<-dp.stepE_dem$self.statistics$mean.resid}


grid ## view grid 

##res.dev.=3.29, lr =0.001, tc=1, nt=600, n=157

## plot chosen gbm.step
ggInfluence(dp.stepE_dem) ## better influence summery - used gbm.step 
ggPD(dp.stepE_dem,rug = T) ## plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.stepE_dem) ## plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.stepE_dem,index = T) ## shows table of interactions-only showing baittype?

# Boostrap the BRT 1000 times to build confidence intervals
dp.stepE_dem.prerun<- plot.gbm.4list(dp.stepE_dem)

dp.stepE_dem.boot <- gbm.bootstrap.functions(dp.stepE_dem, list.predictors=dp.stepE_dem.prerun, n.reps=100)

# Draw PDPs 
PDbootE_dem<-ggPD_boot(dp.stepE_dem, list.4.preds=dp.stepE_dem.prerun, 
                booted.preds=dp.stepE_dem.boot$function.preds, type.ci="ribbon", rug=T)

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.stepE_dem, predictor="Burley", list.4.preds=dp.stepE_dem.prerun, 
                       booted.preds=dp.stepE_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

##Bait Type
bait_boot<-ggPD_boot(dp.stepE_dem, predictor="BaitType", list.4.preds=dp.stepE_dem.prerun, 
                     booted.preds=dp.stepE_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.stepE_dem, predictor="Month.Year", list.4.preds=dp.stepE_dem.prerun, 
                      booted.preds=dp.stepE_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.step_dem, predictor="Boat_ramp", list.4.preds=dp.stepE_dem.prerun, 
                   booted.preds=dp.stepE_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?


## gbm code - might not need to use

dp.gbmE_dem<-gbm(data=dat.east_dem,
                No.FishDP~offset(TotalFishHooked)+Month.Year+Boat_ramp+No.Boats+Lat+Long+fivekm+Kernel.density+SST+
                DisFromBRkm+DecMedian_time+DecFishingHours+GearCost_DP+FishMeth+Burley+BaitType+Max_hook+Boat_Length, ## formula
                distribution = "poisson", ## distribution of response 
                n.trees=1050,  ## number of trees 
                interaction.depth=1, ## tree complexity
                shrinkage=0.1, ## learning rate 
                bag.fraction=0.75,  ## bag fraction
                verbose=TRUE) ## print table

sink("dp.gbmE_dem.summary.txt")
par(mar=c(4, 10, 1, 1))
summary(dp.gbmE_dem, las=1)##show relative influence table and plot
sink()
pretty.gbm.tree(dp.gbmE_dem, i.tree=1) ## 

par(mfrow=c(1,1)) 
plot.gbm(dp.gbmE_dem, i.var=c(1:12)) ### doesnt like vector 
dismo::gbm.plot.fits(dp.gbmE_dem)

## manual version of what i attempted above - wont go into matrix
plot.gbm(dp.gbmE_dem, i.var=c(1),ylab="No. Fish DP") ## Month/year
plot.gbm(dp.gbmE_dem, i.var=c(2),ylab="No. Fish DP") ## BR
plot.gbm(dp.gbmE_dem, i.var=c(3),ylab="No. Fish DP") ## no.boats - as expected
plot.gbm(dp.gbmE_dem, i.var=c(4),ylab="No. Fish DP") ## Lat
plot.gbm(dp.gbmE_dem, i.var=c(5),ylab="No. Fish DP") ## Long
plot.gbm(dp.gbmE_dem, i.var=c(6),ylab="No. Fish DP") ## five km
plot.gbm(dp.gbmE_dem, i.var=c(7),ylab="No. Fish DP") ## kernel density - opposite to expectation
plot.gbm(dp.gbmE_dem, i.var=c(8),ylab="No. Fish DP") ## SST 
plot.gbm(dp.gbmE_dem, i.var=c(9),ylab="No. Fish DP") ## DisFromBRkm - as expected 
plot.gbm(dp.gbmE_dem, i.var=c(10),ylab="No. Fish DP") ## median time 
plot.gbm(dp.gbmE_dem, i.var=c(11),ylab="No. Fish DP") ## fishing hours - opposite to expectation
plot.gbm(dp.gbmE_dem, i.var=c(12),ylab="No. Fish DP") ## gear cost - as expected 
plot.gbm(dp.gbmE_dem, i.var=c(13),ylab="No. Fish DP") ## FishMeth 
plot.gbm(dp.gbmE_dem, i.var=c(14),ylab="No. Fish DP") ## burley
plot.gbm(dp.gbmE_dem, i.var=c(15),ylab="No. Fish DP") ## bait
plot.gbm(dp.gbmE_dem, i.var=c(16),ylab="No. Fish DP") ## omax hook - posite to expectation
plot.gbm(dp.gbmE_dem, i.var=c(17),ylab="No. Fish DP") ### boat length -opposite to expectation

## Interactions
### code to find if there are any interactions in data set
dp.gbmE_dem.inter <- dismo::gbm.interactions(dp.gbmE_dem) ##undefined cols selcted - no col argument? 

## seeing strnegth of interactions
dp.gbmE_dem.inter<-interact.gbm(dp.gbmE_dem, data=dat.east_dem, i.var =(1:12)) # interaction depth too low in model call
                    
#### Both - demersal ---------------------------------------------------

## subset to demersal fishing
dat_dem<- subset(dat, FishMeth%in%c("Bottom-bashing (drifting)", "Bottom-bashing (anchored)"))

summary(dat_dem)## summary of data
head(dat_dem, 3) ## First 3 rows in each col
str(dat_dem) ## Chech data types
names(dat_dem) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid.dem<-expand.grid(lr=c(0.01, 0.5, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid.dem$res.dev<-NA ## add red.dev to grid
grid.dem## check grid

###use gbm.step to find optimal trees with a range of lr and tc combinations 
for(i in 1:nrow(grid)) {
    dp.step_dem<-dismo::gbm.step(data=dat_dem,
                           gbm.x=c(1:9, 12, 13, 15:19), ## explanatory
                           gbm.y=c(10),## response
                           lr=grid[i,"lr"], ## ref to grid 
                           tc=grid[i,"tc"], ## ref to grid 
                           family="poisson", ## distribution family
                           step.size=5,
                           bag.fraction = 0.5)
grid.dem[i, "res.dev"]<-dp.step_dem$self.statistics$mean.resid}

grid.dem## view grid 


##res.dev=3.56, lr =0.5, tc=5, nt=830, bf=0.5

## plot chose gbm.step 
ggInfluence(dp.step_dem) ### better influence summery - used gbm.step 
ggPD(dp.step_dem,rug = T) #### plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.step_dem) ### plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.step_dem,index = T) ## shows table of interactions-only showing baittype?

## Boostrap the BRT 1000 times to build confidence intervals
dp.step_dem.prerun<- plot.gbm.4list(dp.step_dem)

dp.step_dem.boot <- gbm.bootstrap.functions(dp.step_dem, list.predictors=dp.step_dem.prerun, n.reps=100)

## Draw PDPs with Cis
PDboot_dem<-ggPD_boot(dp.step_dem, list.4.preds=dp.step_dem.prerun, 
                booted.preds=dp.step_dem.boot$function.preds, type.ci="ribbon", rug=T)

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.step_dem, predictor="Burley", list.4.preds=dp.gbm_dem.prerun, 
                       booted.preds=dp.gbm_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

##Bait Type
bait_boot<-ggPD_boot(dp.step_dem, predictor="BaitType", list.4.preds=dp.gbm_dem.prerun, 
                     booted.preds=dp.gbm_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.step_dem, predictor="Month.Year", list.4.preds=dp.gbm_dem.prerun, 
                      booted.preds=dp.gbm_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.step_dem, predictor="Boat_ramp", list.4.preds=dp.gbm_dem.prerun, 
                   booted.preds=dp.gbm_dem.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## gbm code - might not need to use 

dp.gbm_dem<-gbm(data=dat_dem,
            No.FishDP~offset(TotalFishHooked)+Month.Year+Boat_ramp+No.Boats+Lat+Long+fivekm+Kernel.density+SST+
            DisFromBRkm+DecMedian_time+DecFishingHours+GearCost_DP+FishMeth+Burley+BaitType+Max_hook+Boat_Length, ## formula
            distribution = "poisson", ## distribution of response 
            n.trees=1050,  ## number of trees 
            interaction.depth=1, ## tree complexity
            shrinkage=0.1, ## learning rate 
            bag.fraction=0.75,  ## bag fraction
            verbose=TRUE) ## print table

sink("dp.gbm_dem.summary.txt")
par(mar=c(4, 10, 1, 1))
summary(dp.gbm_dem, las=1)##show relative influence table and plot
sink()
pretty.gbm.tree(dp.gbm_dem, i.tree=1) ## 

par(mfrow=c(1,1))
plot.gbm(dp.gbm_dem, i.var=c(1:12)) ### doesnt like vector 
dismo::gbm.plot.fits(dp.gbm_dem)

## manual version of what i attempted above - wont go into matrix
plot.gbm(dp.gbm_dem, i.var=c(1),ylab="No. Fish DP") ## Month/year
plot.gbm(dp.gbm_dem, i.var=c(2),ylab="No. Fish DP") ## BR
plot.gbm(dp.gbm_dem, i.var=c(3),ylab="No. Fish DP") ## no.boats - as expected
plot.gbm(dp.gbm_dem, i.var=c(4),ylab="No. Fish DP") ## Lat
plot.gbm(dp.gbm_dem, i.var=c(5),ylab="No. Fish DP") ## Long
plot.gbm(dp.gbm_dem, i.var=c(6),ylab="No. Fish DP") ## five km
plot.gbm(dp.gbm_dem, i.var=c(7),ylab="No. Fish DP") ## kernel density - opposite to expectation
plot.gbm(dp.gbm_dem, i.var=c(8),ylab="No. Fish DP") ## SST 
plot.gbm(dp.gbm_dem, i.var=c(9),ylab="No. Fish DP") ## DisFromBRkm - as expected 
plot.gbm(dp.gbm_dem, i.var=c(10),ylab="No. Fish DP") ## median time 
plot.gbm(dp.gbm_dem, i.var=c(11),ylab="No. Fish DP") ## fishing hours - opposite to expectation
plot.gbm(dp.gbm_dem, i.var=c(12),ylab="No. Fish DP") ## gear cost - as expected 
plot.gbm(dp.gbm_dem, i.var=c(13),ylab="No. Fish DP") ## FishMeth 
plot.gbm(dp.gbm_dem, i.var=c(14),ylab="No. Fish DP") ## burley
plot.gbm(dp.gbm_dem, i.var=c(15),ylab="No. Fish DP") ## bait
plot.gbm(dp.gbm_dem, i.var=c(16),ylab="No. Fish DP") ## omax hook - posite to expectation
plot.gbm(dp.gbm_dem, i.var=c(17),ylab="No. Fish DP") ### boat length -opposite to expectation

## Interactions
### code to find if there are any interactions in data set
dp.gbm_dem.inter <- dismo::gbm.interactions(dp.gbm_dem) ##undefined cols selcted - no col argument? 

## seeing strnegth of interactions
dp.gbm_dem.inter<-interact.gbm(dp.gbm_dem, data=dat_dem, i.var =(1:12)) # interaction depth too low in model call

#### West - all fishing ---------------------------------------------------

## subset to west
dat.west<- subset(dat, Boat_ramp%in%c("Tantabiddi", "Coral Bay"))

summary(dat.west)## summary of data
head(dat.west, 3) ## First 3 rows in each col
str(dat.west) ## Chech data types
names(dat.west) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.001, 0.05, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid## check grid

###use gbm.step to find optimal trees with a range of lr and tc combinations 
for(i in 1:nrow(grid)) {
  dp.stepW<-dismo::gbm.step(data=dat.west,
                                gbm.x=c(1:9, 12, 13, 15:19), ## explanatory
                                gbm.y=c(10),## response
                                lr=grid[i,"lr"], ## ref to grid 
                                tc=grid[i,"tc"], ## ref to grid 
                                family="poisson", ## distribution family
                                step.size=5,
                                bag.fraction=0.5) 
  grid[i, "res.dev"]<-dp.stepW$self.statistics$mean.resid}
grid  ## view grid 

##n=250, res.dev=2.63, lr =0.1, tc=3, nt=490

## plot chosen gbm.step model 
ggInfluence(dp.stepW) ### better influence summery - used gbm.step 
ggPD(dp.stepW,rug = T) #### plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.stepW) ### plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.stepW,index = T) ## shows table of interactions-only showing baittype?

## Boostrap the BRT 1000 times to build confidence intervals
dp.stepW.prerun<- plot.gbm.4list(dp.stepW)

dp.stepW.boot <- gbm.bootstrap.functions(dp.stepW, list.predictors=dp.stepW.prerun, n.reps=100)

## Draw PDPs with cis
PDbootW<-ggPD_boot(dp.stepW, list.4.preds=dp.stepW.prerun, 
                booted.preds=dp.stepW.boot$function.preds, type.ci="ribbon", rug=T)

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.stepW, predictor="Burley", list.4.preds=dp.stepW.prerun, 
                       booted.preds=dp.stepE.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

##Bait Type
bait_boot<-ggPD_boot(dp.stepW, predictor="BaitType", list.4.preds=dp.stepW.prerun, 
                     booted.preds=dp.stepW.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.stepW, predictor="Month.Year", list.4.preds=dp.stepW.prerun, 
                      booted.preds=dp.stepW.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.stepW, predictor="Boat_ramp", list.4.preds=dp.stepW.prerun, 
                   booted.preds=dp.stepW.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## gbm code - might not need to use 
dp.gbmW<-gbm(data=dat.west,
                No.FishDP~offset(TotalFishHooked)+Month.Year+Boat_ramp+No.Boats+Lat+Long+fivekm+Kernel.density+SST+
                  DisFromBRkm+DecMedian_time+DecFishingHours+GearCost_DP+FishMeth+Burley+BaitType+Max_hook+Boat_Length, ## formula
                distribution = "poisson", ## distribution of response 
                n.trees=1050,  ## number of trees 
                interaction.depth=1, ## tree complexity
                shrinkage=0.1, ## learning rate 
                bag.fraction=0.75,  ## bag fraction
                verbose=TRUE) ## print table

sink("dp.gbmW.summary.txt")
par(mar=c(4, 10, 1, 1))
summary(dp.gbmW, las=1)##show relative influence table and plot
sink()
pretty.gbm.tree(dp.gbmW, i.tree=1) ## 

par(mfrow=c(1,1))
plot.gbm(dp.gbmW, i.var=c(1:12)) ### doesnt like vector 
dismo::gbm.plot.fits(dp.gbmW)

## manual version of what i attempted above - wont go into matrix
plot.gbm(dp.gbmW, i.var=c(1),ylab="No. Fish DP") ## Month/year
plot.gbm(dp.gbmW, i.var=c(2),ylab="No. Fish DP") ## BR
plot.gbm(dp.gbmW, i.var=c(3),ylab="No. Fish DP") ## no.boats - as expected
plot.gbm(dp.gbmW, i.var=c(4),ylab="No. Fish DP") ## Lat
plot.gbm(dp.gbmW, i.var=c(5),ylab="No. Fish DP") ## Long
plot.gbm(dp.gbmW, i.var=c(6),ylab="No. Fish DP") ## five km
plot.gbm(dp.gbmW, i.var=c(7),ylab="No. Fish DP") ## kernel density - opposite to expectation
plot.gbm(dp.gbmW, i.var=c(8),ylab="No. Fish DP") ## SST 
plot.gbm(dp.gbmW, i.var=c(9),ylab="No. Fish DP") ## DisFromBRkm - as expected 
plot.gbm(dp.gbmW, i.var=c(10),ylab="No. Fish DP") ## median time 
plot.gbm(dp.gbmW, i.var=c(11),ylab="No. Fish DP") ## fishing hours - opposite to expectation
plot.gbm(dp.gbmW, i.var=c(12),ylab="No. Fish DP") ## gear cost - as expected 
plot.gbm(dp.gbmW, i.var=c(13),ylab="No. Fish DP") ## FishMeth 
plot.gbm(dp.gbmW, i.var=c(14),ylab="No. Fish DP") ## burley
plot.gbm(dp.gbmW, i.var=c(15),ylab="No. Fish DP") ## bait
plot.gbm(dp.gbmW, i.var=c(16),ylab="No. Fish DP") ## omax hook - posite to expectation
plot.gbm(dp.gbmW, i.var=c(17),ylab="No. Fish DP") ### boat length -opposite to expectation

## Interactions
### code to find if there are any interactions in data set
dp.gbmW.inter <- dismo::gbm.interactions(dp.gbmW) ##undefined cols selcted - no col argument? 

## seeing strnegth of interactions
dp.gbmW.inter<-interact.gbm(dp.gbmW, data=dat.west, i.var =(1:12)) # interaction depth too low in model call

#### East - all fishing  ---------------------------------------------------

## subset to west
dat.east<- subset(dat, Boat_ramp%in%c("Bundegi", "Exmouth marina"))

summary(dat.east)## summary of data
head(dat.east, 3) ## First 3 rows in each col
str(dat.east) ## Chech data types
names(dat.east) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.001, 0.05, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid## check grid

###use gbm.step to find optimal trees with a range of lr and tc combinations 
sink("dp.stepE.txt")
for(i in 1:nrow(grid)) {
  dp.stepE<-dismo::gbm.step(data=dat.east,
                            gbm.x=c(1:9, 12, 13, 15:19), ## explanatory
                            gbm.y=c(10),## response
                            lr=grid[i,"lr"], ## ref to grid 
                            tc=grid[i,"tc"], ## ref to grid 
                            family="poisson", ## distribution family
                            step.size=5,
                            bag.fraction=0.5) 
  grid[i, "res.dev"]<-dp.stepE$self.statistics$mean.resid}
sink()
grid  ## view grid 

##n=157, res.dev.=2.8, lr =0.1, tc=5, nt=900

## plot chosen gbm.step model
ggInfluence(dp.stepE) ### better influence summery - used gbm.step 
ggPD(dp.stepE,rug = T) #### plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.stepE) ### plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.stepE,index = T) ## shows table of interactions-only showing baittype?

# Boostrap the BRT 1000 times to build confidence intervals
dp.stepE.prerun<- plot.gbm.4list(dp.stepE)

dp.stepE.boot <- gbm.bootstrap.functions(dp.stepE, list.predictors=dp.stepE.prerun, n.reps=100)

# Draw PDPs with Cis
PDbootE<-ggPD_boot(dp.stepE, list.4.preds=dp.stepE.prerun, 
                booted.preds=dp.stepE.boot$function.preds, type.ci="ribbon", rug=T)

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.stepE, predictor="Burley", list.4.preds=dp.stepE.prerun, 
                       booted.preds=dp.stepE.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## Bait Type
bait_boot<-ggPD_boot(dp.stepE, predictor="BaitType", list.4.preds=dp.stepE.prerun, 
                     booted.preds=dp.stepE.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.stepE, predictor="Month.Year", list.4.preds=dp.stepE.prerun, 
                      booted.preds=dp.stepE.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.stepE, predictor="Boat_ramp", list.4.preds=dp.stepE.prerun, 
                   booted.preds=dp.stepE.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## gbm code - might not need to use
dp.gbmE<-gbm(data=dat.east,
             No.FishDP~offset(TotalFishHooked)+Month.Year+Boat_ramp+No.Boats+Lat+Long+fivekm+Kernel.density+SST+
             DisFromBRkm+DecMedian_time+DecFishingHours+GearCost_DP+FishMeth+Burley+BaitType+Max_hook+Boat_Length, ## formula
             distribution = "poisson", ## distribution of response 
             n.trees=1050,  ## number of trees 
             interaction.depth=1, ## tree complexity
             shrinkage=0.1, ## learning rate 
             bag.fraction=0.75,  ## bag fraction
             verbose=TRUE) ## print table

sink("dp.gbmE.summary.txt")
par(mar=c(4, 10, 1, 1))
summary(dp.gbmE, las=1)##show relative influence table and plot
sink()
pretty.gbm.tree(dp.gbmE, i.tree=1) ## 

par(mfrow=c(1,1))
plot.gbm(dp.gbmE, i.var=c(1:12)) ### doesnt like vector 
dismo::gbm.plot.fits(dp.gbmE)

## manual version of what i attempted above - wont go into matrix
plot.gbm(dp.gbmE, i.var=c(1),ylab="No. Fish DP") ## Month/year
plot.gbm(dp.gbmE, i.var=c(2),ylab="No. Fish DP") ## BR
plot.gbm(dp.gbmE, i.var=c(3),ylab="No. Fish DP") ## no.boats - as expected
plot.gbm(dp.gbmE, i.var=c(4),ylab="No. Fish DP") ## Lat
plot.gbm(dp.gbmE, i.var=c(5),ylab="No. Fish DP") ## Long
plot.gbm(dp.gbmE, i.var=c(6),ylab="No. Fish DP") ## five km
plot.gbm(dp.gbmE, i.var=c(7),ylab="No. Fish DP") ## kernel density - opposite to expectation
plot.gbm(dp.gbmE, i.var=c(8),ylab="No. Fish DP") ## SST 
plot.gbm(dp.gbmE, i.var=c(9),ylab="No. Fish DP") ## DisFromBRkm - as expected 
plot.gbm(dp.gbmE, i.var=c(10),ylab="No. Fish DP") ## median time 
plot.gbm(dp.gbmE, i.var=c(11),ylab="No. Fish DP") ## fishing hours - opposite to expectation
plot.gbm(dp.gbmE, i.var=c(12),ylab="No. Fish DP") ## gear cost - as expected 
plot.gbm(dp.gbmE, i.var=c(13),ylab="No. Fish DP") ## FishMeth 
plot.gbm(dp.gbmE, i.var=c(14),ylab="No. Fish DP") ## burley
plot.gbm(dp.gbmE, i.var=c(15),ylab="No. Fish DP") ## bait
plot.gbm(dp.gbmE, i.var=c(16),ylab="No. Fish DP") ## omax hook - posite to expectation
plot.gbm(dp.gbmE, i.var=c(17),ylab="No. Fish DP") ### boat length -opposite to expectation

## Interactions
## code to find if there are any interactions in data set
dp.gbmE.inter <- dismo::gbm.interactions(dp.gbmE) ##undefined cols selcted - no col argument? 

## seeing strnegth of interactions
dp.gbmE.inter<-interact.gbm(dp.gbmE, data=dat.east, i.var =(1:12)) # interaction depth too low in model call

#### Both - all fishing DP model  ---------------------------------------------------

summary(dat)## summary of data
head(dat, 3) ## First 3 rows in each col
str(dat) ## Chech data types
names(dat) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.001, 0.05, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid## check grid

###use gbm.step to find optimal trees with a range of lr and tc combinations 
for(i in 1:nrow(grid)) {
  dp.step<-dismo::gbm.step(data=dat,
                            gbm.x=c(1:8, 10:18)), ## explanatory 
                            gbm.y=c(9),## response. 
                             ## offset=
                            lr=grid[i,"lr"], ## ref to grid 
                            tc=grid[i,"tc"], ## ref to grid 
                            step.size=50,
                            family="poisson")## distribution family
 grid[i, "res.dev"]<-dp.step$self.statistics$mean.resid ### store res.dev in grid 
  } ### close loop

grid  ## view grid 

## dp: chose lr (0.05) and tc (1) with lowest res.dev: 1.81

dp.gbm<-dismo::gbm.step(data=dat,
                           gbm.x=c(1:8, 10:18), ## explanatory
                           gbm.y=c(9),## response
                           lr=0.05, ## ref to grid 
                           tc=1, ## ref to grid 
                           step.size=50,
                           family="poisson")## distribution family 

### nt=1250

## plot chosen gbm.step model
ggInfluence(dp.gbm) ### better influence summery - used gbm.step 
ggPD(dp.gbm, rug = T) #### plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(dp.gbm) ### plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(dp.gbm,index = T) ## shows table of interactions-only showing baittype?

# Boostrap the BRT 1000 times to build confidence intervals
dp.gbm.prerun<- plot.gbm.4list(dp.gbm)

dp.gbm.boot <- gbm.bootstrap.functions(dp.gbm, list.predictors=dp.gbm.prerun, n.reps=1000)

# Draw PDPs with Cis 
PDP.gbm<-ggPD_boot(dp.gbm, list.4.preds=dp.gbm.prerun, 
                booted.preds=dp.gbm.boot$function.preds, type.ci="ribbon", rug=T) 

## factor variables ##

## PDP Burley
burley_boot<-ggPD_boot(dp.step, predictor="Burley", list.4.preds=dp.step.prerun, 
                       booted.preds=dp.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## Bait Type
bait_boot<-ggPD_boot(dp.step, predictor="BaitType", list.4.preds=dp.step.prerun, 
                     booted.preds=dp.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(dp.step, predictor="Month.Year", list.4.preds=dp.step.prerun, 
                      booted.preds=dp.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(dp.step, predictor="Boat_ramp", list.4.preds=dp.step.prerun, 
                   booted.preds=dp.step.boot$function.preds) 


#### Both - all fishing CR model --------------------------------------

summary(dat)## summary of data
head(dat, 3) ## First 3 rows in each col
str(dat) ## Chech data types
names(dat) ## view names of cols and col number

## making a grid for loop
res.dev<-c() ## make a res.dev variable to store in grid
grid<-expand.grid(lr=c(0.001, 0.05, 0.1), tc=c(1, 3, 5)) ## make grid of lr and tc that gbm.step will run through 
grid$res.dev<-NA ## add red.dev to grid
grid## check grid

###use gbm.step to find optimal trees with a range of lr and tc combinations 
for(i in 1:nrow(grid)) {
  cr.step<-dismo::gbm.step(data=dat,
                           gbm.x=c(1:9, 11:18), ## explanatory
                           gbm.y=c(10),## response
                           ## offset=
                           lr=grid[i,"lr"], ## ref to grid 
                           tc=grid[i,"tc"], ## ref to grid 
                           step.size=50,
                           family="poisson")## distribution family
  grid[i, "res.dev"]<-cr.step$self.statistics$mean.resid ### store res.dev in grid 
} ### close loop

grid  ## view grid 

## cr: chosse lr (0.05) and tc (1) with lowest res.dev: 5.07

cr.gbm<-dismo::gbm.step(data=dat,
                        gbm.x=c(1:9, 11:18), ## explanatory
                        gbm.y=c(10),## response
                        lr=0.05, ## ref to grid 
                        tc=1, ## ref to grid 
                        step.size=50,
                        family="poisson")## distribution family 

### nt=900

## plot chosen gbm.step model
ggInfluence(cr.gbm) ### better influence summery - used gbm.step 
ggPD(cr.gbm, rug = T) #### plots PDP for fitted functions  no CI for every variable in a matrix
ggPDfit(cr.gbm) ### plots PDPs for fitted value no CI for evey variable in a matrix 
ggInteract_list(cr.gbm,index = T) ## shows table of interactions-only showing baittype?

# Boostrap the BRT 1000 times to build confidence intervals
cr.gbm.prerun<- plot.gbm.4list(cr.gbm)

cr.gbm.boot <- gbm.bootstrap.functions(cr.gbm, list.predictors=cr.gbm.prerun, n.reps=1000)

# Draw PDPs with Cis 
PDP.gbm<-ggPD_boot(cr.gbm, list.4.preds=cr.gbm.prerun, 
                   booted.preds=cr.gbm.boot$function.preds, type.ci="ribbon", rug=T) 

## factor variables plots that arnt working 

## PDP Burley
burley_boot<-ggPD_boot(cr.step, predictor="Burley", list.4.preds=cr.step.prerun, 
                       booted.preds=cr.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## Bait Type
bait_boot<-ggPD_boot(cr.step, predictor="BaitType", list.4.preds=cr.step.prerun, 
                     booted.preds=cr.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Month.Year
Month_boot<-ggPD_boot(cr.step, predictor="Month.Year", list.4.preds=cr.step.prerun, 
                      booted.preds=cr.step.boot$function.preds, type.ci = "ribbon",rug = T) 
##### do you need to adjust aesthetic?

## PDP Boat_ramp
BR_boot<-ggPD_boot(cr.step, predictor="Boat_ramp", list.4.preds=cr.step.prerun, 
                   booted.preds=cr.step.boot$function.preds) 

#### Spatial Packages ---------------------------------------------------

library(dplyr)
install.packages("rgdal")
library(rgdal)
library(sp)
install.packages("sf")
library(sf)
install.packages("raster")
library(raster)
install.packages("rgeos")
library(rgeos)
install.packages("classInt", dependencies=TRUE)
library(classInt)
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("tmap") ### didnt work 
library(tmap)
install.packages("leaflet")
library(leaflet)

#### Spatial data ---------------------------------------------------
aus_sf<-st_read("/Users/Nicolita/Documents/UWA PhD/QGIS files & Maps/WA/CoastlineLGATE_070/CoastlineLGATE_070.shp")
aus_sf 
str(aus_sf)
plot(aus_sf)