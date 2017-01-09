#This function finds work and home locations of each user
#----------------------------------------------------------------------------------------------
#Required Libraries: leaderCluster, dplyr
#----------------------------------------------------------------------------------------------
#Required Columns in the Data (mallReq): session_id, lat, lng, mall_id, day, day_name
#----------------------------------------------------------------------------------------------

findHomeAndWork <- function(mallReq, activeDaysLimit) {

#Tuning Parameters
latLongAccuracy <- 3 # 1 = 11.1km, 2 = 1.1km, 3 = 110m
clusterRadius <- 0.1 #cluster tuning parameter (in km)
workHourEventBegin <- 13
workHourEventEnd <- 17
homeHourEventBegin <- 19
homeHourEventEnd <- 7

#Find Active Users and Filter Out Others
activeUsers <- mallReq %>%
  filter(is.na(lat) == FALSE & is.na(lng)==FALSE) %>%
  group_by(session_id)%>%
  summarize(totActiveDays = n_distinct(day))
activeUsers <- filter(activeUsers, totActiveDays>=activeDaysLimit)

print(paste0("...Finding home & work locations for ", nrow(activeUsers), " users who passed minimum ", activeDaysLimit, " days limit..."))
Sys.sleep(1.5)
#--------------------------------------------------------------

notEnoughDataUserCnt <- 0

for (userN in 1:nrow(activeUsers)){
  singleUserData <- filter(mallReq, session_id==activeUsers$session_id[userN])
  #singleUserData <- filter(mallReq, session_id==148192419646175)
  
  #Find GPS Blocks and Unique Days Spent at Each Block
  singleUserData$latlongGroup <- paste0(round(singleUserData$lat, digits=latLongAccuracy),"_", round(singleUserData$lng, digits=latLongAccuracy)) #digits 2 = 0.7 mile/1.1km accuracy
  gpsBlocks <- singleUserData %>%
    group_by(session_id, latlongGroup) %>%
    summarize(gps_days = n_distinct(day), avgLat = mean(lat), avgLng = mean(lng)) %>%
    arrange(session_id, desc(gps_days))
  
  #Filter Out NAs & 1 day blocks (transitional data)
  gpsBlocks <- na.omit(gpsBlocks)
  gpsBlocks <- filter(gpsBlocks, gps_days>1) # should be implemented per person by %
  if(nrow(gpsBlocks) == 0){
    notEnoughDataUserCnt <- notEnoughDataUserCnt + 1
    next #Skip round
  } 
    
  #Find Clusters & Merge to Mall Request Table
  out = leaderCluster(points = gpsBlocks[,c("avgLat", "avgLng")], radius = clusterRadius, distance = "haversine")
  gpsBlocks$cluster_id <- out$cluster_id
  
  #Find Cluster Stats (tower days, duratio, work hour events & )
  singleUserData <- merge(singleUserData, gpsBlocks, by=c("session_id", "latlongGroup"))
  
  singleUserData$hour <- as.numeric(singleUserData$hour)
  singleUserData$workEvent <- 0
  singleUserData$homeEvent <- 0
  singleUserData$workEvent[singleUserData$hour>=workHourEventBegin & singleUserData$hour<workHourEventEnd & singleUserData$day_name != "Saturday" & singleUserData$day_name != "Sunday"] <- 1
  singleUserData$homeEvent[singleUserData$hour>=homeHourEventBegin | singleUserData$hour<homeHourEventEnd] <- 1
  
  clusterStats <- singleUserData %>%
    group_by(session_id, cluster_id)%>%
    summarize(clusterDays = n_distinct(day), clusterDuration = max(day) - min(day), homeEventCount = sum(homeEvent), workEventCount = sum(workEvent))
  
  clusterStats <- merge(clusterStats, activeUsers, by="session_id")
  
  #Cluster Stats Normalization --IMPORTANCE WEIGHTS ARE ADJUSTED HERE--
  clusterStats$clusterDaysNORM <- as.numeric(clusterStats$clusterDays)/as.numeric(clusterStats$totActiveDays)
  clusterStats$clusterDurationNORM <- as.numeric(clusterStats$clusterDuration)/as.numeric(clusterStats$totActiveDays)
  clusterStats$homeEventNORM <- as.numeric(clusterStats$homeEventCount)/max(clusterStats$homeEventCount)
  clusterStats$homeEventNORM[is.na(clusterStats$homeEventNORM)] <- 0
  clusterStats$workEventNORM <- as.numeric(clusterStats$workEventCount)/max(clusterStats$workEventCount)
  clusterStats$workEventNORM[is.na(clusterStats$workEventNORM)] <- 0
  
  clusterStats$clusterImportanceScore <- 0.5*clusterStats$clusterDurationNORM + 0.5*clusterStats$clusterDaysNORM
  clusterStats$clusterHomeImportanceScore <- 0.5*clusterStats$clusterImportanceScore + 0.5*clusterStats$homeEventNORM
  clusterStats$clusterWorkImportanceScore <- 0.5*clusterStats$clusterImportanceScore + 0.5*clusterStats$workEventNORM
  
  #Assign Home & Work Clusters
  home_cluster_score <- max(clusterStats$clusterHomeImportanceScore)
  home_cluster <- clusterStats$cluster_id[clusterStats$clusterHomeImportanceScore == home_cluster_score] 
  work_cluster_score <- max(clusterStats$clusterWorkImportanceScore)
  work_cluster <- clusterStats$cluster_id[clusterStats$clusterWorkImportanceScore == work_cluster_score] 
  
  #HOME
  userHomeLocation <- singleUserData %>%
    filter(cluster_id == home_cluster) %>%
    group_by(session_id) %>%
    summarize(home_lat = mean(lat), home_long = mean(lng))
  
  homeMallList <- unique(filter(singleUserData, (mall_id > 0 & is.na(mall_id) == FALSE & cluster_id == home_cluster))$mall_id)
  
  userHomeLocation$mallidNearHome <- 0
  if(length(homeMallList)==0){
    homeMallMerged <- 0
  }else if (length(homeMallList)==1){
    homeMallMerged <- homeMallList[1]
  }else if  (length(homeMallList)>1) {
    homeMallMerged <- homeMallList[1]
    
    for(i in 2 : length(homeMallList))
        homeMallMerged <- paste0(homeMallMerged, "_", homeMallList[i])
  }
  userHomeLocation$mallidNearHome <- homeMallMerged
  
  #WORK
  userWorkLocation <- singleUserData %>%
    filter(cluster_id == work_cluster) %>%
    group_by(session_id) %>%
    summarize(work_lat = mean(lat), work_long = mean(lng))
  
  workMallList <- unique(filter(singleUserData, (mall_id > 0 & is.na(mall_id) == FALSE & cluster_id == work_cluster))$mall_id)
  
  userWorkLocation$mallidNearWork <- 0
  if(length(workMallList)==0){
    workMallMerged <- 0
  }else if (length(workMallList)==1){
    workMallMerged <- workMallList[1]
  }else if  (length(workMallList)>1) {
    workMallMerged <- workMallList[1]
    
    for(i in 2 : length(workMallList))
      workMallMerged <- paste0(workMallMerged, "_", workMallList[i])
  }
  userWorkLocation$mallidNearWork <- workMallMerged
  
  #Merge HOME & WORK
  userHomeWork <- merge(userHomeLocation, userWorkLocation, by="session_id")
  userHomeWork$home_office_dist <- round (distm(userHomeWork[1,c("work_long", "work_lat")], userHomeWork[1, c("home_long", "home_lat")])/1000, digits=2)
  
  #Merge All User Data
  if(userN == 1){
    allUserHomeWork <- userHomeWork
  }else{
    allUserHomeWork <- rbind(allUserHomeWork, userHomeWork)
    }
  
}

print(paste0("Users Eliminated due to not enough data: ", notEnoughDataUserCnt))
Sys.sleep(1.5)

allUserHomeWork$home_office_dist <- as.numeric(allUserHomeWork$home_office_dist)
return(allUserHomeWork)

}