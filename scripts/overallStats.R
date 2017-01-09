overallStats <- function(session, mallReq,locReqShop, rezervinOnly) {
  ###############################################################
  #Overall Stats
  ###############################################################
  textplot("OVERALL STATS", col="#D95F02")
  
  #User Counts
  uniqueUsers <- session %>%  summarize(uniqueCount=n_distinct(macMobileId))
  uniqueUsers_mac <- session %>%  summarize(uniqueCount=n_distinct(mac))
  uniqueUsers_mobileID <- session %>%  summarize(uniqueCount=n_distinct(mobile_id))
  
  if (rezervinOnly == FALSE){
    
    #Active Users in last 24hrs & 12hrs
    aDayAgo <- Sys.time() - as.difftime(1, unit="days")
    halfDayAgo <- Sys.time() - as.difftime(0.5, unit="days")
    mallReq_24hrs <- filter(mallReq, timestamp > aDayAgo)
    mallReq_12hrs <- (filter(mallReq_24hrs, timestamp > halfDayAgo))
    
    cntmallReq <- nrow(mallReq)
    cntSessionID <- n_distinct(mallReq$session_id)
    cntmallReq_24hrs <- nrow(mallReq_24hrs)
    cntSessionID_24hrs <- n_distinct(mallReq_24hrs$session_id)
    cntmallReq_12hrs <- nrow(mallReq_12hrs)
    cntSessionID_12hrs <- n_distinct(mallReq_12hrs$session_id)
    
    #Users w/ find location request
    nUniqueUsersLocReq <- n_distinct(locReqShop$session_id)
    nUniqueUsersShop <- n_distinct(locReqShop$session_id[!is.na(locReqShop$shop_id)])
    nUniqueMallLocReq <- n_distinct(locReqShop$mall_id)

    textplot(paste0("OVERALL STATS:\n\n", 
                  "# Unique Users (Mac & Mobile ID) : ", uniqueUsers[[1]],
                  "\n# Unique Macs : ", uniqueUsers_mac[[1]],
                  "\n# Unique Mobile IDs : ", uniqueUsers_mobileID[[1]],
                  "\n\n# Mall Requests :", cntmallReq,
                  "\n# Unique Session IDs w/ Mall Requests : ", cntSessionID,
                  "\n\n# Unique Users(session_id) w/ find location request: ", nUniqueUsersLocReq, 
                  "\n# Unique Users(session_id) Inside Shop: ", nUniqueUsersShop,
                  "\n# Unique Malls(session_id) Visited: ", nUniqueMallLocReq,
                  "\n\n\nLAST 24 HOURS:",
                  "\n\n# Mall Requests : ", cntmallReq_24hrs, " -> (",cntmallReq_24hrs,"/4 = ", round(cntmallReq_24hrs/4), ")",
                  "\n# Unique Session IDs w/ Mall Requests : ", cntSessionID_24hrs ,
                  "\n\n\nLAST 12 HOURS:",
                  "\n\n# Mall Requests : ", cntmallReq_12hrs, " -> (",cntmallReq_12hrs,"/4 = ", round(cntmallReq_12hrs/4), ")",
                  "\n# Unique Session IDs w/ Mall Requests : ", cntSessionID_12hrs) ,
           valign = "top", halign = "left", cex=textplot_size)
  }
  
  #Rezervin Report Only
  textplot(paste0("OVERALL STATS:\n\n", 
                  "\n# Devices Logged In With Rezervin Account: ", uniqueUsers_mobileID[[1]],
                  "\n# Devices with Android 5.1 or Earlier: ", uniqueUsers_mac[[1]],
                  "\n# Uniquely Identifiable Devices (Logged In & Not Logged In): ", uniqueUsers[[1]]) ,
           valign = "top", halign = "left", cex=textplot_size)
}