nearbyVisitIndoorStats <- function(mallReq, locReqShop, mallList, allUserHomeWork, max_mallid, min_restaurant, max_restaurant, activeMalls, rezervinOnly){
  
  textplot("NEARBY - VISIT - INDOOR STATS", col="#D95F02")
  
  nearbyColor <- "firebrick2"; visitColor <- "chocolate2"; indoorColor <- "forestgreen"
  
  ###############################################################
  #Tuning Parameters for "Work/home Detection"Determine Activity Type"
  ###############################################################
  passByLimit <- 15 #mins
  residenceLimit <- 1440 #1 day limit
  
  workBegin <- 6; workEnd <- 24; residenceBegin <- 24; residenceEnd <- 6 #hour
  residenceLength <- 120; workLength <- 300 #mins spent between hours
  
  confidenceTimes <- 1 # number of times work/residence limit above exceeded to qualify for a residence/work
  
  #Indoor Visit Minimum Length
  minIndoorMins <- 10
  
  ###############################################################
  #Pre-Processing
  ###############################################################
  #Mall Requests with real malls & restaurants only
  mallRestReq <- mallReq %>%
    filter( (mall_id > 0 & mall_id < max_mallid) | (mall_id > min_restaurant & mall_id < max_restaurant) ) %>%
    arrange(desc(session_id), timestamp)
  
  #Location Requests with MR>0 only -> Real Indoor Visits
  mallRestLocReq <- locReqShop %>%
    filter(match_ratio>0 & mall_id > 0 & x >=0 & y >= 0 & is.na(mall_id) == FALSE)
  
  #Compute Sessions Info (cluster sessions)
  mallRestReq <- addSessions(mallRestReq, 180, "mall")
  mallRestLocReq <- addSessions(mallRestLocReq, 180, "mall")
  
  #Session Analysis - VISIT (group sessions)
  sessionTypes <- mallRestReq %>%
    group_by(sessionNo, session_id) %>%
    summarize(sessionLength = as.numeric(difftime(strptime(max(timestamp), "%Y-%m-%d %H:%M:%S"), strptime(min(timestamp), "%Y-%m-%d %H:%M:%S"), units="mins")), 
              sessionBeginTime = min(timestamp), sessionEndTime = max(timestamp), avgLat = mean(lat), avgLong = mean(lng))
  sessionTypes$beginHour <- as.numeric(substr(sessionTypes$sessionBeginTime, start = 12, stop = 13))
  sessionTypes$endHour <- as.numeric(substr(sessionTypes$sessionEndTime, start = 12, stop = 13))
  
  #Session Analysis - INDOOR (group sessions)
  sessionTypesIndoor <- mallRestLocReq %>%
    group_by(sessionNo, session_id) %>%
    summarize(sessionLength = as.numeric(difftime(strptime(max(timestamp), "%Y-%m-%d %H:%M:%S"), strptime(min(timestamp), "%Y-%m-%d %H:%M:%S"), units="mins")), 
              sessionBeginTime = min(timestamp), sessionEndTime = max(timestamp), avgLat = mean(lat), avgLong = mean(lng))
  sessionTypesIndoor$beginHour <- as.numeric(substr(sessionTypesIndoor$sessionBeginTime, start = 12, stop = 13))
  sessionTypesIndoor$endHour <- as.numeric(substr(sessionTypesIndoor$sessionEndTime, start = 12, stop = 13))
  
  #(DETERMINE ACTIVITY TYPE)------------------------------
  sessionTypes$activity <- NA
  for (n in 1:nrow(sessionTypes)){
    if(sessionTypes$sessionLength[n] < passByLimit)
      sessionTypes$activity[n] <- "PassBy"
    else if(sessionTypes$sessionLength[n] >= residenceLimit) # Those who exceed 24hrs in same mallid
      sessionTypes$activity[n] <- "Residence"
    else if(sessionTypes$beginHour[n] <= residenceEnd & sessionTypes$sessionLength[n] >= residenceLength) # Those who started btw. 0-6am and spent 2+ hrs (**including 6:00 - 8:00**)
      sessionTypes$activity[n] <- "Residence"
    else if(sessionTypes$beginHour[n] > residenceEnd & (residenceBegin - sessionTypes$beginHour[n]) + (residenceLength/60) <= sessionTypes$sessionLength[n]/60 ) # Those who started btw. 6-23 and spent 2+ hrs during the 0-6 range
      sessionTypes$activity[n] <- "Residence"
    else if(sessionTypes$beginHour[n] > workBegin & (sessionTypes$endHour[n] < workEnd | sessionTypes$endHour[n] < 3) &  sessionTypes$sessionLength[n] >= workLength) # Those who started btw. 6-23 and spent 2+ hrs during the 0-6 range
      sessionTypes$activity[n] <- "Work"
    else
      sessionTypes$activity[n] <- "Visit"   
  }
  #--------------------------------------------------------------------------
  
  mallRestReq <- merge(x=mallRestReq, y=sessionTypes, by=c("sessionNo", "session_id"), all.x=TRUE)
  mallRestLocReq <- merge(x=mallRestLocReq, y=sessionTypesIndoor, by=c("sessionNo", "session_id"), all.x=TRUE)
  
  ###############################################################
  #Nearby, Visit & Indoor Unique Mall Visits
  ###############################################################
  sessionLifetime <- mallReq %>%
    group_by(session_id)%>%
    summarize(duration = max(daysSinceInstall))
  
  max_duration <- max(as.numeric(sessionLifetime$duration), na.rm=TRUE)
  avg_duration <- round(mean(as.numeric(sessionLifetime$duration), na.rm=TRUE))
  
  twoWeekActiveSessions <- sessionLifetime %>% filter(duration >= 14 & is.na(duration) == FALSE)
  
  #Nearby Stats
  userMallVisit <- mallReq %>%
    filter(mall_id > 0 & mall_id < max_mallid) %>%
    group_by(session_id) %>%
    summarize(uniqueMallVisits = n_distinct(mall_id))
  
  userMallVisit <- userMallVisit %>%
    group_by(uniqueMallVisits) %>%
    summarize(uniqueUserCount = n())
  
  pNearby1 <- ggplot(data=userMallVisit, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Nearby Malls", "\n\nTotal # People Nearby Malls: ", sum(userMallVisit$uniqueUserCount))) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + labs(caption="(Within 500m Range & Including People Passing By, Working/Residing and Visiting Malls/Restaurants)") +
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + theme(legend.position="none") +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(userMallVisit$uniqueMallVisits),1)) + scale_fill_manual(values=nearbyColor) +
    theme(axis.text=element_text(size=12.5),axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=nearbyColor))
  print(pNearby1)
  
  #Visit Stats
  userMallVisit2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(session_id, sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  
  avgDurationMallVisit <- round(mean(userMallVisit2$avgDuration))
  
  userMallVisit2 <- userMallVisit2 %>%
    group_by(session_id) %>%
    summarize(totMallVisits = n(), uniqueMallVisits = n_distinct(mall_id), avgDuration = mean(avgDuration) )
  
  userMallVisit2 <- userMallVisit2 %>%
    group_by(uniqueMallVisits) %>%
    summarize(uniqueUserCount = n(), avgNumVisits = mean(totMallVisits), avgDuration = round(mean(avgDuration)))
  
  userMallVisit2$visitPerMall = round(userMallVisit2$avgNumVisits / userMallVisit2$uniqueMallVisits, digits=1)
  
  pVisit1 <- ggplot(data=userMallVisit2, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Mall Visits", "\n\nTotal # People Visited Malls: ", sum(userMallVisit2$uniqueUserCount),
                   "\nMax Days Since Install: ", max_duration, " days", " | Avg. Days Since Install: ", avg_duration, " days",
                   "\nAverage Visit Duration: ", avgDurationMallVisit, " mins")) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + theme(legend.position="none") + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount,"\n(", avgDuration, " mins/visit)\n(",visitPerMall, " visits/mall)" )), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(userMallVisit2$uniqueMallVisits),1)) + scale_fill_manual(values=visitColor) +
    theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=visitColor))
  print(pVisit1)
  
  #Visit Stats: First 2 weeks after install
  mallReqTwoWeeks <- mallRestReq %>%
    filter(session_id %in% twoWeekActiveSessions$session_id & daysSinceInstall <= 14) %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(session_id, sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  avgDurationMallVisit2 <- round(mean(mallReqTwoWeeks$avgDuration))
  
  mallReqTwoWeeks <- mallReqTwoWeeks %>%
    group_by(session_id) %>%
    summarize(totMallVisits = n(), uniqueMallVisits = n_distinct(mall_id), avgDuration = mean(avgDuration) )
  
  mallReqTwoWeeks <- mallReqTwoWeeks %>%
    group_by(uniqueMallVisits) %>%
    summarize(uniqueUserCount = n(), avgNumVisits = mean(totMallVisits), avgDuration = round(mean(avgDuration)))
  
  mallReqTwoWeeks$visitPerMall = round(mallReqTwoWeeks$avgNumVisits / mallReqTwoWeeks$uniqueMallVisits, digits=1)
  
  pVisit2Weeks <- ggplot(data=mallReqTwoWeeks, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Mall Visits (First 2 Weeks After Install)\n\n Avg. Visit Duration: ", avgDurationMallVisit2, " mins")) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + theme(legend.position="none") + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount,"\n(", avgDuration, " mins/visit)\n(",visitPerMall, " visits/mall)" )), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(mallReqTwoWeeks$uniqueMallVisits),1)) + scale_fill_manual(values=visitColor) +
    theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=visitColor))
  print(pVisit2Weeks)
  
  #Indoor Stats
  mallVisitIndoor <- mallRestLocReq %>%
    filter(sessionLength>minIndoorMins) %>%
    group_by(session_id, sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  
  avgDurationMallIndoor <- round(mean(mallVisitIndoor$avgDuration))
  
  mallVisitIndoor <- mallVisitIndoor %>%
    group_by(session_id) %>%
    summarize(totMallVisits = n(), uniqueMallVisits = n_distinct(mall_id), avgDuration = mean(avgDuration) )
  
  mallVisitIndoor <- mallVisitIndoor %>%
    group_by(uniqueMallVisits) %>%
    summarize(uniqueUserCount = n(), avgNumVisits = mean(totMallVisits), avgDuration = round(mean(avgDuration)))
  
  mallVisitIndoor$visitPerMall = round(mallVisitIndoor$avgNumVisits / mallVisitIndoor$uniqueMallVisits, digits=1)
  
  pIndoor1 <- ggplot(data=mallVisitIndoor, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Mall Indoor", "\n\nTotal # People Visited Malls (Indoor): ", sum(mallVisitIndoor$uniqueUserCount),
                   "\nMax Days Since Install: ", max_duration, " days", " | Avg. Days Since Install: ", avg_duration, " days",
                   "\nAverage Visit Duration: ", avgDurationMallVisit, " mins")) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + theme(legend.position="none") +
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + 
    labs(caption=paste0("(Detected by Mall IQ Indoor Location Technology. Including People Who Were Inside a Given Mall For At Least ", minIndoorMins, " mins)")) +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount,"\n(", avgDuration, " mins/visit)\n(",visitPerMall, " visits/mall)" )), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(mallVisitIndoor$uniqueMallVisits),1)) + scale_fill_manual(values=indoorColor) +
    theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=indoorColor))
  print(pIndoor1)
  
  ###############################################################
  #Top 20: Mall & Restaurant Nearby,Visit & Indoor Tables - (Rezervin Report)
  ###############################################################
  #Nearby Mall
  textplot("Nearby Mall Stats: Top 20 (Unique Users)")
  
  mallStats <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid) %>%
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueNearby = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  mallStats <- merge(mallStats, mallList[,c("mall_id", "name")], by="mall_id")
  mallStats$totalNearbyHours <- round(mallStats$mallReqCount/4)
  mallStats <- mallStats[,c("name", "uniqueUsers", "totalNearbyHours", "uniqueNearby")]
  mallStats <- mallStats %>% arrange(desc(uniqueUsers))
  
  mallStats$avgDuration <- round(mallStats$totalNearbyHours / mallStats$uniqueNearby,digits=2)
  mallStats$avgNumVisit <- round(mallStats$uniqueNearby / mallStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(mallStats[1:20,], theme = ttheme_default(base_colour = nearbyColor))
  
  textplot("Nearby Mall Stats: Top 20 (Total Visit Hours)")
  
  mallStats <- mallStats %>% arrange(desc(totalNearbyHours))
  grid.newpage()
  grid.table(mallStats[1:20,], theme = ttheme_default(base_colour = nearbyColor))
  
  #Nearby Restaurant
  textplot("Nearby Restaurant Stats: Top 20 (Unique Users)")
  
  restaurantStats <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant) %>%
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueNearby = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  restaurantStats <- merge(restaurantStats, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantStats$totalNearbyHours <- round(restaurantStats$mallReqCount/4)
  restaurantStats <- restaurantStats[,c("name", "uniqueUsers", "totalNearbyHours", "uniqueNearby")]
  restaurantStats <- restaurantStats %>% arrange(desc(uniqueUsers))
  
  restaurantStats$avgDuration <- round(restaurantStats$totalNearbyHours / restaurantStats$uniqueNearby,digits=2)
  restaurantStats$avgNumVisit <- round(restaurantStats$uniqueNearby / restaurantStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(restaurantStats[1:20,], theme = ttheme_default(base_colour = nearbyColor))
  
  textplot("Nearby Restaurant Stats: Top 20 (Total Visit Hours)")
  
  restaurantStats <- restaurantStats %>% arrange(desc(totalNearbyHours))
  grid.newpage()
  grid.table(restaurantStats[1:20,], theme = ttheme_default(base_colour = nearbyColor))
  
  #--------------------------------------------------------------
  
  #Visit Mall
  textplot("Visit Mall Stats: Top 20 (Unique Users)")
  
  mallStats <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueVisits = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  mallStats <- merge(mallStats, mallList[,c("mall_id", "name")], by="mall_id")
  mallStats$totalVisitHours <- round(mallStats$mallReqCount/4)
  mallStats <- mallStats[,c("name", "uniqueUsers", "totalVisitHours", "uniqueVisits")]
  mallStats <- mallStats %>% arrange(desc(uniqueUsers))
  
  mallStats$avgDuration <- round(mallStats$totalVisitHours / mallStats$uniqueVisits,digits=2)
  mallStats$avgNumVisit <- round(mallStats$uniqueVisits / mallStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(mallStats[1:20,], theme = ttheme_default(base_colour = visitColor))
  
  textplot("Visit Mall Stats: Top 20 (Total Visit Hours)")
  
  mallStats <- mallStats %>% arrange(desc(totalVisitHours))
  grid.newpage()
  grid.table(mallStats[1:20,], theme = ttheme_default(base_colour = visitColor))
  
  #Visit Restaurant
  textplot("Visit Restaurant Stats: Top 20 (Unique Users)")
  
  restaurantStats <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueNearby = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  restaurantStats <- merge(restaurantStats, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantStats$totalNearbyHours <- round(restaurantStats$mallReqCount/4)
  restaurantStats <- restaurantStats[,c("name", "uniqueUsers", "totalNearbyHours", "uniqueNearby")]
  restaurantStats <- restaurantStats %>% arrange(desc(uniqueUsers))
  
  restaurantStats$avgDuration <- round(restaurantStats$totalNearbyHours / restaurantStats$uniqueNearby,digits=2)
  restaurantStats$avgNumVisit <- round(restaurantStats$uniqueNearby / restaurantStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(restaurantStats[1:20,], theme = ttheme_default(base_colour = visitColor))
  
  textplot("Visit Restaurant Stats: Top 20 (Total Visit Hours)")
  
  restaurantStats <- restaurantStats %>% arrange(desc(totalNearbyHours))
  grid.newpage()
  grid.table(restaurantStats[1:20,], theme = ttheme_default(base_colour = visitColor))
  
  #--------------------------------------------------------------
  
  #Indoor Mall
  textplot(paste0("Indoor Mall Stats: Top 20 (Unique Users)\n(Displaying Active ", length(activeMalls), " Malls Only)"))
  
  mallStats <- mallRestLocReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & mall_id %in% activeMalls) %>% #displaying active malls
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueVisits = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  mallStats <- merge(mallStats, mallList[,c("mall_id", "name")], by="mall_id")
  mallStats$totalVisitHours <- round(mallStats$mallReqCount/4)
  mallStats <- mallStats[,c("name", "uniqueUsers", "totalVisitHours", "uniqueVisits")]
  mallStats <- mallStats %>% arrange(desc(uniqueUsers))
  
  mallStats$avgDuration <- round(mallStats$totalVisitHours / mallStats$uniqueVisits,digits=2)
  mallStats$avgNumVisit <- round(mallStats$uniqueVisits / mallStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(mallStats[1:length(activeMalls),], theme = ttheme_default(base_colour = indoorColor))
  
  textplot(paste0("Indoor Mall Stats: Top 20 (Total Visit Hours)\n(Displaying Active ", length(activeMalls), " Malls Only)"))
  
  mallStats <- mallStats %>% arrange(desc(totalVisitHours))
  grid.newpage()
  grid.table(mallStats[1:length(activeMalls),], theme = ttheme_default(base_colour = indoorColor))
  
  ###############################################################
  #Mall & Restaurant Visitors
  ###############################################################
  #Mall Nearby
  mallVisits <- mallReq %>%
    filter(mall_id > 0 & mall_id < max_mallid) %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  mallVisits <- merge(mallVisits, mallList[,c("mall_id", "name")], by="mall_id")
  mallVisits <- mallVisits %>% arrange(desc(uniqueUserCount))
  
  ptopMallNearby <- ggplot(data=mallVisits[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10  Malls (Nearby)")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=nearbyColor) +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=nearbyColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")

  #Mall Visit
  mallVisits2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  mallVisits2b <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  mallVisits2b <- mallVisits2b %>%
    group_by(mall_id)%>%
    summarize(numVisits = n(), avgDuration=round(mean(avgDuration)))
  mallVisits2 <- merge(mallVisits2, mallVisits2b, by="mall_id")
    
  mallVisits2 <- merge(mallVisits2, mallList[,c("mall_id", "name")], by="mall_id")
  mallVisits2 <- mallVisits2 %>% arrange(desc(uniqueUserCount))
  
  top10Malls <- mallVisits2$mall_id[1:10]
  
  ptopMallVisit <- ggplot(data=mallVisits2[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10 Malls (Visit)")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=visitColor) +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins/visit)")), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=visitColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")

  #Mall Indoor
  mallVisitsIndoor <- mallRestLocReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & mall_id %in% activeMalls) %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  mallVisitsIndoor2 <- mallRestLocReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & mall_id %in% activeMalls) %>%
    group_by(sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  mallVisitsIndoor2 <- mallVisitsIndoor2 %>%
    group_by(mall_id)%>%
    summarize(numVisits = n(), avgDuration=round(mean(avgDuration)))
  mallVisitsIndoor <- merge(mallVisitsIndoor, mallVisitsIndoor2, by="mall_id")
  
  mallVisitsIndoor <- merge(mallVisitsIndoor, mallList[,c("mall_id", "name")], by="mall_id")
  mallVisitsIndoor <- mallVisitsIndoor %>% arrange(desc(uniqueUserCount))
  
  ptopMallIndoor <- ggplot(data=mallVisitsIndoor[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10 Malls (Indoor)")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=indoorColor) +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins/visit)")), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=indoorColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")

  #Restaurants Nearby
  restaurantVisits <- mallReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant) %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  restaurantVisits <- merge(restaurantVisits, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantVisits <- restaurantVisits %>% arrange(desc(uniqueUserCount))
  
  ptopRestNearby <- ggplot(data=restaurantVisits[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10 Restaurants (Nearby)")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=nearbyColor) +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=nearbyColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")
  
  #Real Restaurants Visits
  restaurantVisits2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  restaurantVisits2b <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  restaurantVisits2b <- restaurantVisits2b %>%
    group_by(mall_id)%>%
    summarize(numVisits = n(), avgDuration=round(mean(avgDuration)))
  restaurantVisits2 <- merge(restaurantVisits2, restaurantVisits2b, by="mall_id")
  
  restaurantVisits2 <- merge(restaurantVisits2, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantVisits2 <- restaurantVisits2 %>% arrange(desc(uniqueUserCount))
  
  top10Malls <- restaurantVisits2$mall_id[1:10]
  
  
  ptopRestVisit <- ggplot(data=restaurantVisits2[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10 Restaurants (Visit)")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=visitColor) +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins/visit)")), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=visitColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")
  
  grid.arrange(ptopMallNearby,ptopMallVisit,ptopMallIndoor,ptopRestNearby,ptopRestVisit, ncol = 3, top=textGrob("Mall & Restaurant Nearby-Visit-Indoor", gp=gpar(fontsize=20)))
  
  ###############################################################
  #Mall & Restaurant Visits Over Time
  ###############################################################
  mallReqUser2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n()) %>%
    arrange(day, day_hour)
  
  restaurantReqUser2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n()) %>%
    arrange(day, day_hour)
  
  pMall <- ggplot(data=mallReqUser2, aes(x=reorder(day_hour, day),y=mallReqCount), fill=day_name) + 
    ggtitle(paste0("Mall Visits")) + ylab('# Unique User') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
    geom_text(aes(y=mallReqCount, label = mallReqCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=6,angle=90))
  pRestaurant <- ggplot(data=restaurantReqUser2, aes(x=reorder(day_hour, day),y=mallReqCount), fill=day_name) + 
    ggtitle(paste0("Restaurant Visits")) + ylab('# Unique User') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
    geom_text(aes(y=mallReqCount, label = mallReqCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) +
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  
  grid.arrange(pMall,pRestaurant, nrow = 2, top=textGrob("Mall/Restaurant Visits Over Time Since Launch", gp=gpar(fontsize=20)))
  
  ###############################################################
  #Mall & Restaurant Visits for given Hours & Days
  ###############################################################
  mallVisitDayHours2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(day_name, hour) %>%
    summarize(uniqueUserCount = n_distinct(session_id)) %>%
    arrange(desc(uniqueUserCount)) %>%
    group_by(hour) %>%
    mutate(y.cumul=cumsum(uniqueUserCount),
           y.text = lag(y.cumul) + 0.5*(y.cumul - lag(y.cumul)))  
  mallVisitDayHours2$y.text[is.na(mallVisitDayHours2$y.text)] <- 0.5*mallVisitDayHours2$y.cumul[is.na(mallVisitDayHours2$y.text)]
  
  pMallHourDay <- ggplot(data=mallVisitDayHours2, aes(x=hour ,y=uniqueUserCount, group=uniqueUserCount), fill=day_name) + 
    ggtitle(paste0("Mall Visits")) + ylab('# Total Unique Users per Given Hour per Day') + xlab('Hours') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=y.text, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=0)) 
  
  
  restaurantVisitDayHours2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(day_name, hour) %>%
    summarize(uniqueUserCount = n_distinct(session_id)) %>%
    arrange(desc(uniqueUserCount)) %>%
    group_by(hour) %>%
    mutate(y.cumul=cumsum(uniqueUserCount),
           y.text = lag(y.cumul) + 0.5*(y.cumul - lag(y.cumul)))  
  restaurantVisitDayHours2$y.text[is.na(restaurantVisitDayHours2$y.text)] <- 0.5*restaurantVisitDayHours2$y.cumul[is.na(restaurantVisitDayHours2$y.text)]
  
  pRestHourDay<- ggplot(data=restaurantVisitDayHours2, aes(x=hour ,y=uniqueUserCount, group=uniqueUserCount), fill=day_name) + 
    ggtitle(paste0("Restaurant Visits")) + ylab('# Total Unique Users per Given Hour per Day') + xlab('Hours') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=y.text, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=0)) + 
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  
  grid.arrange(pMallHourDay, pRestHourDay, ncol = 1, top=textGrob("Mall/Restaurant Visits For a Given Hour/Day", gp=gpar(fontsize=20)))
  
  ###############################################################
  #Top 10 Malls & Restaurants Details Since Launch & For Geiven day-hour
  ###############################################################
  #PreProcessing. Find Top 10 Malls & Restaurants
  mallVisits2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  mallVisits2 <- mallVisits2 %>% arrange(desc(uniqueUserCount))
  top10Malls <- mallVisits2$mall_id[1:10]
  
  restaurantVisits2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  restaurantVisits2 <- restaurantVisits2 %>% arrange(desc(uniqueUserCount))
  top10Restaurants <- restaurantVisits2$mall_id[1:10]
  
  #Part1. 7 days view only
  top10MallDisplayDay <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit" & mall_id %in% top10Malls) %>% 
    group_by(mall_id, hour, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10MallDisplayDay <- merge(top10MallDisplayDay, mallList[,c("mall_id", "name")], by="mall_id")
  
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Monday"] <- 1
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Tuesday"] <- 2
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Wednesday"] <- 3
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Thursday"] <- 4
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Friday"] <- 5
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Saturday"] <- 6
  top10MallDisplayDay$day_code[top10MallDisplayDay$day_name=="Sunday"] <- 7

  top10MallDisplayDay$day_hour <- paste0(top10MallDisplayDay$day_name, "_", top10MallDisplayDay$hour)
  
  p10Mallw <- ggplot(data=top10MallDisplayDay, aes(x=reorder(day_hour, day_code),y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Mall Visits: Weekly View")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) + facet_grid(name ~ .) + 
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  print(p10Mallw)
  
  
  top10RestDisplayDay <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit" & mall_id %in% top10Restaurants) %>% 
    group_by(mall_id, hour, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10RestDisplayDay <- merge(top10RestDisplayDay, mallList[,c("mall_id", "name")], by="mall_id")
  
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Monday"] <- 1
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Tuesday"] <- 2
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Wednesday"] <- 3
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Thursday"] <- 4
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Friday"] <- 5
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Saturday"] <- 6
  top10RestDisplayDay$day_code[top10RestDisplayDay$day_name=="Sunday"] <- 7
  
  top10RestDisplayDay$day_hour <- paste0(top10RestDisplayDay$day_name, "_", top10RestDisplayDay$hour)
  
  p10Restw <- ggplot(data=top10RestDisplayDay, aes(x=reorder(day_hour, day_code),y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Restaurant Visits: Weekly View")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) + facet_grid(name ~ .) + 
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  print(p10Restw)
  
  #Part2. Mall & Restaurant Since Launch
  top10MallDisplay <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit" & mall_id %in% top10Malls) %>% 
    group_by(mall_id, day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10MallDisplay <- merge(top10MallDisplay, mallList[,c("mall_id", "name")], by="mall_id")
  
  p10Mall <- ggplot(data=top10MallDisplay, aes(x=reorder(day_hour, day),y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Mall Visits Since Launch For Given Days & Hours")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=6,angle=90)) + facet_grid(name ~ .) + 
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  print(p10Mall)
  
  top10RestaurantDisplay <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit" & mall_id %in% top10Restaurants) %>%
    group_by(mall_id, day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10RestaurantDisplay <- merge(top10RestaurantDisplay, mallList[,c("mall_id", "name")], by="mall_id")
  
  p10Rest <- ggplot(data=top10RestaurantDisplay, aes(x=reorder(day_hour, day),y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Restaurant Visits Since Launch For Given Days & Hours")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=6,angle=90)) + facet_grid(name ~ .) +
    labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)")
  print(p10Rest)
}