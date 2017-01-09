
installStats <- function(session, rezervinOnly) {
  if(rezervinOnly == FALSE){
    
    textplot("INSTALL STATS", col="#D95F02")
    
    ###############################################################
    #Install per Hour by SDK Version
    ###############################################################
    #Display Android Installs per Hour by sdk version
    preciseLaunchSession <- session[,c("macMobileId", "timestamp", "info_version_sdk")]
    
    #Add Relative Date/Time
    preciseLaunchSession$timestampSec <- as.integer(as.POSIXct( preciseLaunchSession$timestamp ))
    preciseLaunchSession$relSec <- preciseLaunchSession$timestampSec - min(preciseLaunchSession$timestampSec)
    preciseLaunchSession$relHour <- floor(preciseLaunchSession$relSec/3600)
    preciseLaunchSession$relDay <- as.integer(floor(preciseLaunchSession$relSec/86400))
    
    sdkHourGroup <- preciseLaunchSession %>%
      group_by(relHour, info_version_sdk) %>%
      summarize(count = n(), uniqueCount = n_distinct(macMobileId)) %>%
      arrange(relHour, desc(uniqueCount))
    
    sdkDayGroup <- preciseLaunchSession %>%
      group_by(relDay, info_version_sdk) %>%
      summarize(count = n(), uniqueCount = n_distinct(macMobileId)) %>%
      arrange(relDay, desc(uniqueCount))
    
    #Add Percentages
    sdkTot <- sdkHourGroup %>%
      group_by(relHour)%>%
      summarize(tot = sum(uniqueCount))
    sdkHourGroup <- merge(sdkHourGroup, sdkTot, by="relHour")
    sdkHourGroup$pct <- round(100*sdkHourGroup$uniqueCount/sdkHourGroup$tot, digits=2)
    
    sdkTot <- sdkDayGroup %>%
      group_by(relDay)%>%
      summarize(tot = sum(uniqueCount))
    sdkDayGroup <- merge(sdkDayGroup, sdkTot, by="relDay")
    sdkDayGroup$pct <- round(100*sdkDayGroup$uniqueCount/sdkDayGroup$tot, digits=2)
    
    #Text Position
    sdkHourGroup <- sdkHourGroup %>%
      group_by(relHour) %>%
      mutate(y.cumul=cumsum(uniqueCount),
             y.text = lag(y.cumul) + 0.5*(y.cumul - lag(y.cumul)))  
    sdkHourGroup$y.text[is.na(sdkHourGroup$y.text)] <- 0.5*sdkHourGroup$y.cumul[is.na(sdkHourGroup$y.text)]
    
    sdkDayGroup <- sdkDayGroup %>%
      group_by(relDay) %>%
      mutate(y.cumul=cumsum(uniqueCount),
             y.text = lag(y.cumul) + 0.5*(y.cumul - lag(y.cumul)))  
    sdkDayGroup$y.text[is.na(sdkDayGroup$y.text)] <- 0.5*sdkDayGroup$y.cumul[is.na(sdkDayGroup$y.text)]
    
    p1 <- ggplot(data=sdkHourGroup, aes(x=relHour,y=uniqueCount, group=uniqueCount), fill=info_version_sdk) + 
      ggtitle("Installs After Launch (Hours)") + ylab('# Installs (Session)') + xlab('Hours After Launch (2016-12-16 14:00)') + 
      geom_bar(stat="identity", aes(fill=info_version_sdk), width=0.5 ) + scale_fill_brewer(palette="Set3") +
      geom_text(aes(y=y.text, label = uniqueCount, size=text_size-2)) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p1)
    
    p2 <- ggplot(data=sdkDayGroup, aes(x=as.factor(relDay),y=uniqueCount, group=uniqueCount), fill=info_version_sdk) + 
      ggtitle("Installs After Launch (Days)") + ylab('# Installs (Session)') + xlab('Days After Launch (2016-12-16 14:00)') + 
      geom_bar(stat="identity", aes(fill=info_version_sdk), width=0.5 ) + scale_fill_brewer(palette="Set3") +
      geom_text(aes(y=y.text, label = paste0(pct, "% (" ,uniqueCount, ")")), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p2)
    }
}