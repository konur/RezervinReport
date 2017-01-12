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

#Set Directories
setwd("C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin/RezervinReport/")
scriptsDirectory <- "C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin/RezervinReport/scripts/"
reportsDirectory <- "C:/Users/Konuralp/Dropbox/MallIQ/Code/R_Projects/Rezervin/RezervinReport/reports/"

#Set Report Type
rezervinOnly <- FALSE #TRUE if report is generated for Rezervin / FALSE if for Mall IQ

ptm <- proc.time()
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
source(paste0(scriptsDirectory, "shopStats.R"), chdir=TRUE)
source(paste0(scriptsDirectory, "displayOnMap.R"), chdir=TRUE)

###############################################################
#Load Parameters
###############################################################
#remove scientific notation & stop if there is an error
options(scipen=999); options(error=stop);

#ggplot text sizes & max table length for table display
plot_size <- 16; axis_title_size <- 15; text_size <- 7; text_angle <- 60; textplot_size <- 1.5; tableLength <- 40;

#Max mallid (not included)
max_mallid <- 100; min_restaurant <- 1000; max_restaurant <- 2000;

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

###############################################################
#Generate PDF Report
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
nearbyVisitIndoorStats(mallReq, locReqShop, mallList, allUserHomeWork, max_mallid, min_restaurant, max_restaurant, activeMalls, rezervinOnly)

#Shop Stats
shopStats(locReqShop, mallList, shopList, allUserHomeWork, activeMalls, tableLength, rezervinOnly)

dev.off()
proc.time() - ptm