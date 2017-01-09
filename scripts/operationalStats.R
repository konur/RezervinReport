operationalStats <- function (mallReq, locReqShop, allUserHomeWork, tableLength, rezervinOnly){
  
  if (rezervinOnly == FALSE){
    textplot("OPERATIONAL STATS", col="#D95F02")
    ###############################################################
    #Mall Request Counts
    ###############################################################
    textplot("Top Find Mall Requests\n(ordered by unique session_id count)")
    
    mallStats <- mallReq %>%
      group_by(mall_id) %>%
      summarize(mallReqCount=n(), uniqueSessionIDCount=n_distinct(session_id)) %>%
      arrange(desc(uniqueSessionIDCount))
    
    grid.newpage()
    grid.table(mallStats[1:tableLength,])
    
    # for(n in 1:ceiling(nrow(mallStats)/tableLength)){
    #   grid.newpage()
    #   if(n != ceiling(nrow(mallStats)/tableLength))
    #     grid.table(mallStats[((n-1)*tableLength+1):(n*tableLength),])
    #   else
    #     grid.table(mallStats[((n-1)*tableLength+1):nrow(mallStats),])
    # }
    
    textplot("Top Find Mall Requests\n(ordered by unique mall request count)")
    
    mallStats <- mallStats %>% arrange(desc(mallReqCount))
    grid.newpage()
    grid.table(mallStats[1:tableLength,])
    
    ###############################################################
    #Mall Requests | Wifi Find Mall Requests BY Count & Unique Session ID
    ###############################################################
    mallReqUser <- mallReq %>%
      filter(mall_id > 0 & mall_id < max_mallid) %>%
      group_by(day_hour, day, day_name) %>%
      summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())
    #mallReqUser$reqType <- "MallRequest"
    
    wifiMallReqTime <- mallReq %>%
      filter(mall_id == 0 & is.na(lat)==T & is.na(lng)==T) %>%
      group_by(day_hour, day, day_name) %>%
      summarize(wifiMallReqCount = n(), uniqueSessionID = n_distinct(session_id))
    
    pMR <- ggplot(data=mallReqUser, aes(x=day_hour,y=mallReqCount), fill=day_name) + 
      ggtitle(paste0("")) + ylab('Count') + xlab('day_hour') + 
      geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
      geom_text(aes(y=mallReqCount, label = mallReqCount), size=text_size-4) +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
      theme(axis.text.x = element_text(size=10,angle=90))
    pMR2 <- ggplot(data=mallReqUser, aes(x=day_hour,y=uniqueSessionID), fill=day_name) + 
      ggtitle(paste0("")) + ylab('# Unique Session ID') + xlab('day_hour') + 
      geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
      geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
      theme(axis.text.x = element_text(size=10,angle=90))
    
    pWFMR <- ggplot(data=wifiMallReqTime, aes(x=day_hour,y=wifiMallReqCount), fill=day_name) + 
      ggtitle(paste0("")) + ylab('Count') + xlab('day_hour') + 
      geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
      geom_text(aes(y=wifiMallReqCount, label = wifiMallReqCount), size=text_size-4) +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
      theme(axis.text.x = element_text(size=10,angle=90))
    pWFMR2 <- ggplot(data=wifiMallReqTime, aes(x=day_hour,y=uniqueSessionID), fill=day_name) + 
      ggtitle(paste0("")) + ylab('# Unique Session ID') + xlab('day_hour') + 
      geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set2") +
      geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
      theme(axis.text.x = element_text(size=10,angle=90))
    
  
    grid.arrange(pMR,pMR2, nrow = 2, top=textGrob("Find Mall Requests Over Time", gp=gpar(fontsize=20)))
    grid.arrange(pWFMR,pWFMR2, nrow = 2, top=textGrob("Wi-Fi Find Mall Requests", gp=gpar(fontsize=20)))
    
    ###############################################################
    #Find Location Requests - Count/Unique User per Request, MR0 and MR1plus
    ###############################################################
    mallLocReq <- locReqShop %>%
      filter(is.na(match_ratio)==FALSE & x >= 0 & y >= 0) %>%
      group_by(mall_id) %>%
      summarize(requestCount = n(), requestUniqueUsers = n_distinct(session_id))
    
    matchRatio0 <- locReqShop %>%
      filter(match_ratio==0 & is.na(match_ratio)==FALSE & x >= 0 & y >= 0) %>%
      group_by(mall_id) %>%
      summarize(MR0Count = n(), MR0UniqueUsers = n_distinct(session_id) )
    
    matchRatioLarger0 <- locReqShop %>%
      filter(match_ratio>0 & is.na(match_ratio)==FALSE & x >= 0 & y >= 0) %>%
      group_by(mall_id) %>%
      summarize(MR1PlusCount = n(), MR1PlusUniqueUsers = n_distinct(session_id) )
    
    mallLocReq <- merge(x=mallLocReq, y=matchRatio0, by="mall_id", all.x=T)
    mallLocReq <- merge(x=mallLocReq, y=matchRatioLarger0, by="mall_id", all.x=T)
    
    mallLocReq[is.na(mallLocReq)] <- 0
    
    for(n in 1:ceiling(nrow(mallLocReq)/tableLength)){
      grid.newpage()
      if(n != ceiling(nrow(mallLocReq)/tableLength))
        grid.table(mallLocReq[((n-1)*tableLength+1):(n*tableLength),])
      else
        grid.table(mallLocReq[((n-1)*tableLength+1):nrow(mallLocReq),])
    }
    
    ###############################################################
    #Find Location Requests - MR = 0 VS MR != 0
    ###############################################################
    #Match Ratios
    locReqShop$match_ratioType[is.na(locReqShop$match_ratio)==TRUE] <- "MR = NA"
    locReqShop$match_ratioType[locReqShop$match_ratio==0] <- "MR = 0"
    locReqShop$match_ratioType[locReqShop$match_ratio>0] <- "MR > 0"
    
    matchRatios2 <- locReqShop %>%
      group_by(match_ratioType)%>%
      summarize(count = n(), uniqueUser = n_distinct(session_id))
    
    pMR1 <- ggplot(data=matchRatios2, aes(x=match_ratioType,y=count, fill=match_ratioType)) + 
      ggtitle("Count") + ylab('Count') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratioType), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    pMR2 <- ggplot(data=matchRatios2, aes(x=match_ratioType,y=uniqueUser, fill=match_ratioType)) + 
      ggtitle("Unique Users") + ylab('Unique Users') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratioType), width=0.5 ) + 
      geom_text(aes(y=uniqueUser/2, label = paste0(uniqueUser)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    
    #Match Ratios
    locReqShop <- locReqShop %>%  
      filter(is.na(match_ratio) == FALSE) %>%
      mutate(match_ratio_group = cut(match_ratio,breaks=seq(from=-10, to=max(as.numeric(match_ratio), na.rm=TRUE)+10, by=10), na.rm=TRUE))
    
    matchRatios <- locReqShop %>%
      group_by(match_ratio_group)%>%
      summarize(count = n(), uniqueUser = n_distinct(session_id))
    
    pMR3 <- ggplot(data=filter(matchRatios, match_ratio_group != "(-10,0]"), aes(x=match_ratio_group,y=count, fill=match_ratio_group)) + 
      ggtitle("") + ylab('Count') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratio_group), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    pMR4 <- ggplot(data=filter(matchRatios, match_ratio_group != "(-10,0]"), aes(x=match_ratio_group,y=uniqueUser, fill=match_ratio_group)) + 
      ggtitle("") + ylab('Unique Users') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratio_group), width=0.5 ) + 
      geom_text(aes(y=uniqueUser/2, label = paste0(uniqueUser)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    
    grid.arrange(pMR1,pMR2,pMR3,pMR4, ncol = 2, top=textGrob("Overall Match Ratio Stats", gp=gpar(fontsize=20)))
    
    #Match Ratios for Active Malls
    matchRatiosActiveMalls <- locReqShop %>%
      filter(mall_id %in% activeMalls) %>%
      group_by(mall_id, match_ratio_group)%>%
      summarize(count = n(), uniqueUser = n_distinct(session_id))
    matchRatiosActiveMalls <- merge(matchRatiosActiveMalls, mallList[,c("mall_id", "name")], by="mall_id")
    
    pMR5 <-ggplot(data=filter(matchRatiosActiveMalls, match_ratio_group != "(-10,0]"), aes(x=match_ratio_group,y=count, fill=match_ratio_group)) + 
      ggtitle("") + ylab('Count') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratio_group), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) + 
      facet_grid(name ~ .)
    
    pMR6 <-ggplot(data=filter(matchRatiosActiveMalls, match_ratio_group != "(-10,0]"), aes(x=match_ratio_group,y=uniqueUser, fill=match_ratio_group)) + 
      ggtitle("") + ylab('Unique Users') + xlab('Match Ratio') + 
      geom_bar(stat="identity", aes(fill=match_ratio_group), width=0.5 ) + 
      geom_text(aes(y=uniqueUser/2, label = paste0(uniqueUser)), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) + 
      facet_grid(name ~ .)
    
    grid.arrange(pMR5, pMR6, ncol = 2, top=textGrob("Active Malls Match Ratio Stats", gp=gpar(fontsize=20)))
    
    ###############################################################
    #Battery Drain Rates for People Work/home near Malls
    ###############################################################
    allUserHomeWork$home_office_dist <- as.numeric(allUserHomeWork$home_office_dist )
    peopleNearMalls <- allUserHomeWork %>%
      filter(mallidNearHome != "0" | mallidNearWork != "0")
    
    mallReqBattery <- mallReq %>%
      filter(is.na(chr) == FALSE & is.na(session_id) == FALSE)
    
    mallReqBattery$nearMall <- FALSE
    mallReqBattery$nearMall[mallReqBattery$session_id %in% peopleNearMalls$session_id] <- TRUE
    
    mallReqBattery <- mallReqBattery %>%
      arrange(session_id, timestamp)
    
    mallReqBattery$dayHourCharge <- paste0(mallReqBattery$day_hour, "_", 100*mallReqBattery$chr)
    
    mallReqBatteryGroup <- mallReqBattery %>%
      group_by(session_id, dayHourCharge, nearMall) %>%
      summarize(min_date = min(timestamp), max_date = max(timestamp), charge = 100*mean(chr))%>%
      arrange(session_id, min_date)
    
    #Find Time & Charge Difference
    mallReqBatteryGroup$timeDiff <- NA
    dfLength <- nrow(mallReqBatteryGroup)
    mallReqBatteryGroup$timeDiff[2:dfLength] <- as.numeric(difftime(strptime(mallReqBatteryGroup$min_date[2:dfLength], "%Y-%m-%d %H:%M:%S"), 
                                                             strptime(mallReqBatteryGroup$min_date[1:(dfLength-1)], "%Y-%m-%d %H:%M:%S"), units="mins"))
    
    mallReqBatteryGroup$chargeDiff <- NA
    mallReqBatteryGroup$chargeDiff[2:dfLength] <- as.numeric(mallReqBatteryGroup$charge[2:dfLength] - mallReqBatteryGroup$charge[1:(dfLength-1)])
    
    #Filter Out Large time diffs, positive chargeDiff(charging) &NAs
    mallReqBatteryGroup <- filter(mallReqBatteryGroup, timeDiff < 180 & timeDiff >= 1 & chargeDiff < 0 & is.na(chargeDiff) == FALSE & is.na(timeDiff) == FALSE)
    
    #ChargeRate Per Hour
    mallReqBatteryGroup$drainRate <- round (-1* (mallReqBatteryGroup$chargeDiff / (mallReqBatteryGroup$timeDiff/60)), digits=1)
    #filter out very large drainRates
    mallReqBatteryGroup <- mallReqBatteryGroup %>%
      filter(drainRate<=300) %>%
      mutate(drainRateGroup = floor(drainRate/10))
    
    batteryStats <- mallReqBatteryGroup %>%
      group_by(drainRateGroup, nearMall) %>%
      summarize(count = n(), uniqueUserCount = n_distinct(session_id))
    
    #User Counts
    nearMallUsers <- n_distinct(mallReqBatteryGroup$session_id[mallReqBatteryGroup$nearMall==TRUE])
    otherUsers <- n_distinct(mallReqBatteryGroup$session_id[mallReqBatteryGroup$nearMall==FALSE])
    
    #Counts
    nearMallCount <- sum(batteryStats$count[batteryStats$nearMall == TRUE])
    otherMallCount <- sum(batteryStats$count[batteryStats$nearMall == FALSE])
    
    
    ggplot(data=mallReqBatteryGroup, aes(x = timeDiff, y = chargeDiff)) + 
      ggtitle("Monday") + ylab('# Visitors') + xlab('Avg. Session Length') + theme(legend.position="none") +
      geom_point(aes(color=nearMall), size=2) + scale_color_brewer(palette="Set1") + 
      geom_text(aes(y=mallVisits+1, label = paste0(name), size=text_size)) + 
      theme(axis.text=element_text(size=12), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
  }
  
}