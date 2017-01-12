shopStats <- function(locReqShop, mallList, shopList, allUserHomeWork, activeMalls, tableLength, rezervinOnly){
  textplot("SHOP STATS", col="#D95F02")
  
  indoorColor <- "forestgreen"
  
  #Indoor Visit Minimum Length
  minShopVisitMins <- 1
  
  ###############################################################
  #PreProcessing
  ###############################################################
  #Get Shop Visits & Sessions
  shopReq <- locReqShop %>%
    filter(is.na(shop_id) == FALSE, is.na(mall_id) == FALSE, is.na(floor_id) == FALSE)
  
  shopReq <- addSessions(shopReq, 60, "shop")
  
  #Correct and Match Shop IDs
  shopReq$shop_id_old <- shopReq$shop_id
  
  for(i in 1:nrow(shopReq)){
    charLength <- nchar(shopReq$shop_id[i])
    if(charLength == 1)
      shopReq$shop_id[i] <- paste0("00", shopReq$shop_id[i])
    else if(charLength == 2)
      shopReq$shop_id[i] <- paste0("0", shopReq$shop_id[i])
    else if(charLength > 3)
      print("...Error! Shop ID Longer than 3 digits!...")
  }
  shopReq$shop_id <- paste0(shopReq$mall_id, shopReq$floor_id, shopReq$shop_id)
  
  shopReq <- merge(x=shopReq, y=shopList[,c("shop_id", "name")], by="shop_id", all.x=T)
  
  #Get Session Stats
  sessionTypesShop <- shopReq %>%
    group_by(sessionNo, session_id) %>%
    summarize(sessionLength = as.numeric(difftime(strptime(max(timestamp), "%Y-%m-%d %H:%M:%S"), strptime(min(timestamp), "%Y-%m-%d %H:%M:%S"), units="mins")),
              sessionBeginTime = min(timestamp), sessionEndTime = max(timestamp))
  sessionTypesShop$beginHour <- as.numeric(substr(sessionTypesShop$sessionBeginTime, start = 12, stop = 13))
  sessionTypesShop$endHour <- as.numeric(substr(sessionTypesShop$sessionEndTime, start = 12, stop = 13))
  
  shopReq <- merge(x=shopReq, y=sessionTypesShop, by=c("sessionNo", "session_id"), all.x=TRUE)
  
  ###############################################################
  #Top 10/20 Shops Stats
  ###############################################################  
  shopVisits <- shopReq %>%
    filter(sessionLength>=minShopVisitMins) %>%
    group_by(name) %>%
    summarize(uniqueUserCount = n_distinct(session_id))
  shopVisits2 <- shopReq %>%
    filter(sessionLength>=minShopVisitMins) %>%
    group_by(sessionNo, name) %>%
    summarize(avgDuration = mean(sessionLength))
  shopVisits2 <- shopVisits2 %>%
    group_by(name)%>%
    summarize(numVisits = n(), avgDuration=round(mean(avgDuration)))
  shopVisits <- merge(shopVisits, shopVisits2, by="name")
  shopVisits <- shopVisits %>% arrange(desc(uniqueUserCount))
  shopVisits$visitPerPerson <- round(shopVisits$numVisits / shopVisits$uniqueUserCount, digits=1)
  
  top10shops <- shopVisits$name[1:10]

  ptopShops <- ggplot(data=shopVisits[1:10,], aes(x=reorder(name, desc(uniqueUserCount)),y=uniqueUserCount), fill='') + 
    ggtitle(paste0("Top 10 Shops Visited")) + ylab('# Unique Users') + xlab('') + 
    geom_bar(stat="identity", aes(fill=''), width=0.5 ) + scale_fill_manual(values=indoorColor) +
    geom_text(aes(y=uniqueUserCount/2, label = paste0(uniqueUserCount, "\n(", avgDuration, " mins/visit)" )), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold", color=indoorColor)) +
    theme(axis.text.x = element_text(size=10,angle=30)) + theme(legend.position="none")
  print(ptopShops)
  
  pshopScatter <- ggplot(data=shopVisits[1:20,], aes(x = numVisits, y = avgDuration)) + 
    ggtitle("Top 20 Shop Statistics: Avg. Duration VS. # of Visits") + ylab('Avg. Visit Duration (mins)') + xlab('# of Visits') +
    geom_point(aes(color=''), size=5) + scale_color_brewer(palette="Set1") + geom_text(aes(y=avgDuration+1, label = paste0(name), size=text_size-1)) + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold"))
  print(pshopScatter)
  
  textplot("Top 30 Shops")
  grid.newpage()
  grid.table(shopVisits[1:30,])
  
  
  ###############################################################
  #Weekly View
  ############################################################### 
  top10ShopDisplay <- shopReq %>%
    filter(sessionLength>=minShopVisitMins & name %in% top10shops) %>%
    group_by(name, hour, day_name) %>%
    summarize(uniqueSessionID = n_distinct(session_id), mallReqCount = n())

  top10ShopDisplay$day_code <- 0
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Monday"] <- 1
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Tuesday"] <- 2
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Wednesday"] <- 3
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Thursday"] <- 4
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Friday"] <- 5
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Saturday"] <- 6
  top10ShopDisplay$day_code[top10ShopDisplay$day_name=="Sunday"] <- 7
  
  top10ShopDisplay$day_hour <- paste0(top10ShopDisplay$day_name, "_", top10ShopDisplay$hour)
  
  p10ShopW <- ggplot(data=top10ShopDisplay, aes(x=reorder(day_hour, day_code),y=uniqueSessionID), fill=day_name) + 
    ggtitle(paste0("Top 10 Brand Visits: Weekly View")) + ylab('# Unique Users') + xlab('day_hour') + 
    geom_bar(stat="identity", aes(fill=day_name), width=0.5 ) + scale_fill_brewer(palette="Set3") +
    geom_text(aes(y=uniqueSessionID, label = uniqueSessionID), size=text_size-4) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=axis_title_size,face="bold"), plot.title=element_text(size=plot_size, face="bold")) +
    theme(axis.text.x = element_text(size=10,angle=90)) + facet_grid(name ~ .)
  print(p10ShopW)

}
  