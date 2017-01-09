library(RMySQL)
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(date)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggmap)
library(geosphere)
library(gplots)
library(reshape2)
library(stringr)
library(leaderCluster)

#Enable 4 thread parallel foreach execution
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

setwd("C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin")
scriptsDirectory <- "C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin/scripts/"
reportsDirectory <- "C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin/reports/"

rezervinOnly <- FALSE #TRUE if report is generated Rezervin / FALSE if for Mall IQ
###############################################################
#Load Functions
###############################################################
source(paste0(scriptsDirectory, "addSessions.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "findHomeAndWork.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "restaurant2mall.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "overallStats.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "deviceStats.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "installStats.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "operationalStats.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "nearbyVisitIndoorStats.R"), chdir=TRUE)

###############################################################
#Load Parameters
###############################################################
#remove scientific notation & stop if there is an error
options(scipen=999); options(error=stop);

#ggplot text sizes
plot_size <- 16; axis_title_size <- 15; text_size <- 7; text_angle<-60; textplot_size<-1.5; tableLength <- 40 

#Max mallid (not included)
max_mallid <- 100
min_restaurant <- 1000
max_restaurant <- 2000

#PassBy, Residency and Work Parameters
passByLimit <- 10 #mins
residenceLimit <- 1440 #1 day limit

workBegin <- 6; workEnd <- 24; residenceBegin <- 24; residenceEnd <- 6 #hour
residenceLength <- 120; workLength <- 300 #mins spent between hours

confidenceTimes <- 1 # number of times work/residence limit above exceeded to qualify for a residence/work

#wifi-Enabled Malls
activeMalls <- c(26, 27, 54, 53, 72, 88, 28)

###############################################################
#Download & PreProcess Data
###############################################################
source(paste0(scriptsDirectory, "loadSQLData.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "preProcessing.R"), chdir=TRUE)

#Backup Downloaded Files
#locReqShopBACKUP <- locReqShop; mallListBACKUP <- mallList; mallReqBACKUP <- mallReq; sessionBACKUP <- session;
#Restore Files
#locReqShop <- locReqShopBACKUP; mallList <- mallListBACKUP; mallReq <- mallReqBACKUP; session <- sessionBACKUP;

#Get work and home locations for users
allUserHomeWork <- findHomeAndWork(mallReq, 7)
#write.csv(allUserHomeWork, "userHomeAndWorkLocations.csv")

#Replace some restaurants w/ malls
mallReq <- restaurant2mall(mallReq)

#Mall Requests with real malls & restaurants only
# mallRestReq <- mallReq %>%
#   filter( (mall_id > 0 & mall_id < max_mallid) | (mall_id > min_restaurant & mall_id < max_restaurant) ) %>%
#   arrange(desc(session_id), timestamp)



###############################################################
#Mall Requests Displayed on Map
###############################################################
# #Add Near Mall Indicator
# mallReq$nearMall <- FALSE
# 
# mallReq$nearMall[mallReq$mall_id > 0 & mallReq$mall_id < max_mallid] <- TRUE
# 
# 
# if(exists("turkey.map") == FALSE){
#   turkey.map <- get_map("Turkey", zoom=5)
#   ist.map <- get_map("Istanbul", zoom=9)
#   ank.map <- get_map("Ankara", zoom=10)
#   izmir.map <- get_map("Izmir", zoom=10)
# }
# #Points on Map
# ggmap(turkey.map) + ggtitle("Turkey: Rezervin Location Data (Find Mall Requests)") +
#   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) + 
#   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
# 
# ggmap(ist.map) + ggtitle("Istanbul: Rezervin Location Data (Find Mall Requests)") +
#   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) + 
#   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
# 
# ggmap(ank.map) + ggtitle("Ankara: Rezervin Location Data (Find Mall Requests)") +
#   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) + 
#   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
# 
# ggmap(izmir.map) + ggtitle("Izmir: Rezervin Location Data (Find Mall Requests)") +
#   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) + 
#   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
# 
# #Heat Map
# ggmap(ist.map) + ggtitle("Heat Map: Istanbul") +
#   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")
# 
# ggmap(ank.map) + ggtitle("Heat Map: Ankara") +
#   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")
# 
# ggmap(izmir.map) + ggtitle("Heat Map: Izmir") +
#   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")

###############################################################
#Generate Report
###############################################################
#Start Recording PDF
pdf(paste0(reportsDirectory, "rezervinReport_", Sys.Date(), '.pdf'), width=24, height=13.5)


#Overall Stats
overallStats(session, mallReq, locReqShop, rezervinOnly)

#Install Stats
installStats(session, rezervinOnly)

#Device Stats
deviceStats(session, rezervinOnly)

#Mall & Location Request Table Stats
operationalStats(mallReq, locReqShop, allUserHomeWork, tableLength, rezervinOnly)

#Nearby, Visit & Indoor Stats
nearbyVisitIndoorStats(mallReq, locReqShop, allUserHomeWork, max_mallid, min_restaurant, max_restaurant, rezervinOnly)


dev.off()