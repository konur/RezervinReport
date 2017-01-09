nearbyVisitIndoorStats <- function(mallReq, locReqShop, allUserHomeWork, max_mallid, min_restaurant, max_restaurant, rezervinOnly){
  
  textplot("NEARBY - VISIT - INDOOR STATS", col="#D95F02")
  
  ###############################################################
  #Pre-Processing
  ###############################################################
  #Mall Requests with real malls & restaurants only
  mallRestReq <- mallReq %>%
    filter( (mall_id > 0 & mall_id < max_mallid) | (mall_id > min_restaurant & mall_id < max_restaurant) ) %>%
    arrange(desc(session_id), timestamp)
  
  #Compute Sessions Info (cluster sessions)
  mallRestReq <- addSessions(mallRestReq, 180)
  
  #Session Analysis (group sessions)
  sessionTypes <- mallRestReq %>%
    group_by(sessionNo, session_id) %>%
    summarize(sessionLength = as.numeric(difftime(strptime(max(timestamp), "%Y-%m-%d %H:%M:%S"), strptime(min(timestamp), "%Y-%m-%d %H:%M:%S"), units="mins")), 
              sessionBeginTime = min(timestamp), sessionEndTime = max(timestamp), avgLat = mean(lat), avgLong = mean(lng))
  sessionTypes$beginHour <- as.numeric(substr(sessionTypes$sessionBeginTime, start = 12, stop = 13))
  sessionTypes$endHour <- as.numeric(substr(sessionTypes$sessionEndTime, start = 12, stop = 13))
  
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
  
  # residenceWorkList <- mallRestReq %>%
  #   filter(activity == "Work" | activity == "Residence") %>%
  #   group_by(session_id, mall_id, activity) %>%
  #   summarize(uniqueCount = n_distinct(sessionNo))
  # residenceWorkList <- merge(residenceWorkList, mallList[,c("mall_id", "lat", "lng")])
  # 
  # residenceWorkList <- filter(residenceWorkList, uniqueCount>=confidenceTimes)
  # write.csv(residenceWorkList[,c("session_id", "mall_id", "activity")], "residenceWorkList.csv")
  
  ###############################################################
  #Nearby, Visit & Indoor Unique Mall Visits
  ###############################################################
  #User Install Durations
  #mallReq$daysSinceInstall <- as.Date(mallReq$timestamp) - as.Date(mallReq$installDate)
  #mallRestReq$daysSinceInstall <- as.Date(mallRestReq$timestamp) - as.Date(mallRestReq$installDate)
  
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
  
  ggplot(data=userMallVisit, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Nearby Malls", "\n\nTotal # People Nearby Malls: ", sum(userMallVisit$uniqueUserCount))) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + labs(caption="(Within 500m Range & Including People Passing By, Working/Residing and Visiting Malls/Restaurants)") +
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + theme(legend.position="none") +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(userMallVisit$uniqueMallVisits),1)) +
    theme(axis.text=element_text(size=12.5),axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
  
  #Visit Stats
  userMallVisit2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(session_id, sessionNo, mall_id) %>%
    summarize(avgDuration = mean(sessionLength))
  
  #Average Mall Visit Duration (TOTAL)
  avgDurationMallVisit <- round(mean(userMallVisit2$avgDuration))
  
  userMallVisit2 <- userMallVisit2 %>%
    group_by(session_id) %>%
    summarize(totMallVisits = n(), uniqueMallVisits = n_distinct(mall_id), avgDuration = mean(avgDuration) )
  
  userMallVisit2 <- userMallVisit2 %>%
    group_by(uniqueMallVisits) %>%
    summarize(uniqueUserCount = n(), avgNumVisits = mean(totMallVisits), avgDuration = round(mean(avgDuration)))
  
  userMallVisit2$visitPerMall = round(userMallVisit2$avgNumVisits / userMallVisit2$uniqueMallVisits, digits=1)
  
  ggplot(data=userMallVisit2, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Mall Visits", "\n\nTotal # People Visited Malls: ", sum(userMallVisit2$uniqueUserCount),
                   "\nMax Days Since Install: ", max_duration, " days", " | Avg. Days Since Install: ", avg_duration, " days",
                   "\nAverage Visit Duration: ", avgDurationMallVisit, " mins")) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + theme(legend.position="none") + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount,"\n(", avgDuration, " mins/visit)\n(",visitPerMall, " visits/mall)" )), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(userMallVisit2$uniqueMallVisits),1)) +
    theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
  
  #First 2 weeks after install stats
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
  
  ggplot(data=mallReqTwoWeeks, aes(x=uniqueMallVisits,y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Mall Visits - First 2 Weeks After Install\n\n Avg. Visit Duration: ", avgDurationMallVisit2, " mins")) + 
    ylab('# Unique Users') + xlab(paste0('# Unique Malls')) + theme(legend.position="none") + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + labs(caption="(Within 500m Range & Including People Visiting Malls/Restaurants and Spending At Least 15 mins & Excluding People Passing By, Working/Residing)") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount,"\n(", avgDuration, " mins/visit)\n(",visitPerMall, " visits/mall)" )), size=text_size-2) + 
    scale_x_continuous(breaks=seq(1,max(mallReqTwoWeeks$uniqueMallVisits),1)) +
    theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
  
  ###############################################################
  #Mall & Restaurant Visit Tables - Rezervin Report
  ###############################################################
  textplot("Nearby Mall Stats: Top 20 (Unique Users)")
  
  mallStats <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid) %>%
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
  grid.table(mallStats[1:20,])
  
  textplot("Nearby Mall Stats: Top 20 (Total Visit Hours)")
  
  mallStats <- mallStats %>% arrange(desc(totalVisitHours))
  grid.newpage()
  grid.table(mallStats[1:20,])
  
  
  textplot("Nearby Restaurant Stats: Top 20 (Unique Users)")
  
  restaurantStats <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant) %>%
    group_by(mall_id) %>%
    summarize(mallReqCount=n(), uniqueUsers=n_distinct(session_id), uniqueVisits = n_distinct(sessionNo)) %>%
    arrange(desc(uniqueUsers))
  restaurantStats <- merge(restaurantStats, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantStats$totalVisitHours <- round(restaurantStats$mallReqCount/4)
  restaurantStats <- restaurantStats[,c("name", "uniqueUsers", "totalVisitHours", "uniqueVisits")]
  restaurantStats <- restaurantStats %>% arrange(desc(uniqueUsers))
  
  restaurantStats$avgDuration <- round(restaurantStats$totalVisitHours / restaurantStats$uniqueVisits,digits=2)
  restaurantStats$avgNumVisit <- round(restaurantStats$uniqueVisits / restaurantStats$uniqueUsers,digits=2)
  
  grid.newpage()
  grid.table(restaurantStats[1:20,])
  
  textplot("Nearby Restaurant Stats: Top 20 (Total Visit Hours)")
  
  restaurantStats <- restaurantStats %>% arrange(desc(totalVisitHours))
  grid.newpage()
  grid.table(restaurantStats[1:20,])
  
  ###############################################################
  #Mall & Restaurant Visits Over Time
  ###############################################################
  mallReqUser2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  
  restaurantReqUser2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  
  pMall <- ggplot(data=mallReqUser2, aes(x=day_hour,y=mallReqCount), fill=day_name) + 
    ggtitle(paste0("Mall Visits Over Time\n(0 < mall_id < ",max_mallid ,")")) + ylab('# Unique User') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
    geom_text(aes(y=mallReqCount, label = mallReqCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90))
  pRestaurant <- ggplot(data=restaurantReqUser2, aes(x=day_hour,y=mallReqCount), fill=day_name) + 
    ggtitle(paste0("Restaurant Visits Over Time\n", min_restaurant ," < mall_id < ",max_restaurant ,")")) + ylab('# Unique User') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
    geom_text(aes(y=mallReqCount, label = mallReqCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90))
  
  grid.arrange(pMall,pRestaurant, nrow = 2, top=textGrob("Mall/Restaurant Visits", gp=gpar(fontsize=20)))
  
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
  
  ggplot(data=mallVisits[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill=name) + 
    ggtitle(paste0("Top 10 Malls Nearby")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=as.factor(name)), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=0)) + theme(legend.position="none")
  
  #Real Mall Visits
  mallVisits2 <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id), avgDuration = round(mean(sessionLength)))
  mallVisits2 <- merge(mallVisits2, mallList[,c("mall_id", "name")], by="mall_id")
  mallVisits2 <- mallVisits2 %>% arrange(desc(uniqueUserCount))
  
  top10Malls <- mallVisits2$mall_id[1:10]
  
  ggplot(data=mallVisits2[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill=name) + 
    ggtitle(paste0("Top 10 Malls Visited")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=as.factor(name)), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins)")), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=0)) + theme(legend.position="none")
  
  
  #Restaurant Nearby
  restaurantVisits <- mallReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant) %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  restaurantVisits <- merge(restaurantVisits, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantVisits <- restaurantVisits %>% arrange(desc(uniqueUserCount))
  
  ggplot(data=restaurantVisits[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill=name) + 
    ggtitle(paste0("Top 10 Restaurants Nearby")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=as.factor(name)), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueUserCount/2, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")
  
  #Real Restaurant Visits
  restaurantVisits2 <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit") %>%
    group_by(mall_id) %>%
    summarize(uniqueUserCount = n_distinct(session_id), avgDuration = round(mean(sessionLength)))
  restaurantVisits2 <- merge(restaurantVisits2, mallList[,c("mall_id", "name")], by="mall_id")
  restaurantVisits2 <- restaurantVisits2 %>% arrange(desc(uniqueUserCount))
  
  top10Restaurants <- restaurantVisits2$mall_id[1:10]
  
  ggplot(data=restaurantVisits2[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill=name) + 
    ggtitle(paste0("Top 10 Restaurants Visited")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=as.factor(name)), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins)")), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")
  
  ###############################################################
  #Top 10 Malls & Restaurants Details
  ###############################################################
  top10MallDisplay <- mallRestReq %>%
    filter(mall_id > 0 & mall_id < max_mallid & activity=="Visit" & mall_id %in% top10Malls) %>%
    group_by(mall_id, day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10MallDisplay <- merge(top10MallDisplay, mallList[,c("mall_id", "name")], by="mall_id")
  
  ggplot(data=top10MallDisplay, aes(x=day_hour,y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Malls Visited (Days & Hours)\n(0 < mall_id < ",max_mallid ,")")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) + facet_grid(name ~ .)
  
  top10RestaurantDisplay <- mallRestReq %>%
    filter(mall_id > min_restaurant & mall_id < max_restaurant & activity=="Visit" & mall_id %in% top10Restaurants) %>%
    group_by(mall_id, day_hour, day, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
  top10RestaurantDisplay <- merge(top10RestaurantDisplay, mallList[,c("mall_id", "name")], by="mall_id")
  
  ggplot(data=top10RestaurantDisplay, aes(x=day_hour,y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Restaurants Visited (Days & Hours)\n(0 < mall_id < ",max_mallid ,")")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) + facet_grid(name ~ .)
  
  ###############################################################
  #Mall & Restaurant Visit Hours
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
  
  ggplot(data=mallVisitDayHours2, aes(x=hour ,y=uniqueUserCount, group=uniqueUserCount), fill=day_name) + 
    ggtitle(paste0("Mall Visit Hours & Days")) + ylab('# Unique Users per Given Hour per Day') + xlab('Hours') + 
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
  
  ggplot(data=restaurantVisitDayHours2, aes(x=hour ,y=uniqueUserCount, group=uniqueUserCount), fill=day_name) + 
    ggtitle(paste0("Restaurant Visit Hours & Days")) + ylab('# Unique Users per Given Hour per Day') + xlab('Hours') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=y.text, label = uniqueUserCount), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=0))
  
}