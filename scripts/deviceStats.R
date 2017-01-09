deviceStats <- function(session, rezervinOnly) {
  if(rezervinOnly == FALSE){
    textplot("DEVICE STATS", col="#D95F02")
    
    ###############################################################
    #Device Type Count
    ###############################################################
    #Device Manufacturers
    deviceManufacturers <- session %>%
      group_by(macMobileId, info_manufacturer) %>%
      summarize(count = n())
    deviceManufacturers <- deviceManufacturers %>%
      group_by(info_manufacturer) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    deviceManufacturers$count_pct <- round(100*deviceManufacturers$count/sum(deviceManufacturers$count), digits=1)
    
    p1 <- ggplot(data=deviceManufacturers, aes(x=reorder(info_manufacturer,count),y=count), fill=info_manufacturer) + 
      ggtitle("Device Types/Brands") + ylab('# Devices') + xlab('Manufacturer') + 
      geom_bar(stat="identity", aes(fill=info_manufacturer), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count_pct, '%\n(', count, ')')), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p1)
    
    #Device Models (Top 100)
    deviceModels <- session %>%
      group_by(macMobileId,info_manufacturer, info_model) %>%
      summarize(count = n())
    
    deviceModels$deviceModel <- paste0(deviceModels$info_manufacturer, '_', deviceModels$info_model)
    
    deviceModels <- deviceModels %>%
      group_by(info_manufacturer, info_model) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    textplot("Top 120 Devices")
    max_limit <- 120 #Table Length to print
    for(n in 1:ceiling(max_limit/tableLength)){
      grid.newpage()
      if(n != ceiling(max_limit/tableLength))
        grid.table(deviceModels[((n-1)*tableLength+1):(n*tableLength),])
      else
        grid.table(deviceModels[((n-1)*tableLength+1):max_limit,])
    }
    
    ###############################################################
    #SDK Version Count
    ###############################################################
    sdkVersion <- session %>%
      group_by(macMobileId, info_version_sdk) %>%
      summarize(count = n())
    sdkVersion <- sdkVersion %>%
      group_by(info_version_sdk) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    sdkVersion$count_pct <- round(100*sdkVersion$count/sum(sdkVersion$count), digits=1)
    
    p2 <- ggplot(data=sdkVersion, aes(x=reorder(info_version_sdk,count),y=count), fill=info_version_sdk) + 
      ggtitle("SDK Versions") + ylab('# Devices') + xlab('SDK Version') + 
      geom_bar(stat="identity", aes(fill=info_version_sdk), width=0.5 ) + scale_fill_brewer(palette="Set3") +
      geom_text(aes(y=count/2, label = paste0(count_pct, '%\n(', count, ')')), size=text_size-2)+
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p2)
    
    ###############################################################
    #GPS & Network
    ###############################################################
    session$gpsNetwork <- paste0(session$info_gps, '_' , session$info_network)
    gpsNetwork <- session %>%
      group_by(macMobileId, gpsNetwork) %>%
      summarize(count = n())
    
    gpsNetwork <- gpsNetwork %>%
      group_by(gpsNetwork) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    gpsNetwork$count_pct <- round(100*gpsNetwork$count/sum(gpsNetwork$count), digits=1)
    
    p3 <- ggplot(data=gpsNetwork, aes(x=reorder(gpsNetwork,count),y=count), fill=gpsNetwork) + 
      ggtitle("GPS & Network Info") + ylab('# Devices') + xlab('gpsInfo_NetworkInfo') + 
      geom_bar(stat="identity", aes(fill=gpsNetwork), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count_pct, '%\n(', count, ')')), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p3)
    
    ###############################################################
    #Bluetooth
    ###############################################################
    bleInfo <- session %>%
      group_by(macMobileId, info_ble) %>%
      summarize(count = n())
    
    bleInfo <- bleInfo %>%
      group_by(info_ble) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    bleInfo$count_pct <- round(100*bleInfo$count/sum(bleInfo$count), digits=1)
    
    p4 <- ggplot(data=bleInfo, aes(x=reorder(info_ble,count),y=count), fill=info_ble) + 
      ggtitle("Bluetooth Info") + ylab('# Devices') + xlab('Bluetooth Info') + 
      geom_bar(stat="identity", aes(fill=info_ble), width=0.5 ) + 
      geom_text(aes(y=count/2, label = paste0(count_pct, '%\n(', count, ')')), size=text_size-2) +
      theme(axis.text=element_text(size=12.5), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
    print(p4)
  }
}