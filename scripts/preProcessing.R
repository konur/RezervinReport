#Add Numeric Date to Session Table
session$numDate <- as.numeric(paste(substr(session$timestamp, start = 1, stop = 4), substr(session$timestamp, start = 6, stop = 7), 
                                    substr(session$timestamp, start = 9, stop = 10), substr(session$timestamp, start = 12, stop = 13), substr(session$timestamp, start = 15, stop = 16),
                                    substr(session$timestamp, start = 18, stop = 19),  sep=""))
#Add Additional Date Fields to Mall Request Table
mallReq$date <- as.Date(mallReq$timestamp)
mallReq$day  <- mallReq$date - min(mallReq$date)
mallReq$hour <- substr(mallReq$timestamp, start = 12, stop = 13)
mallReq$day_hour <- paste0(mallReq$day,'_',mallReq$hour )
mallReq$day_name <- weekdays(as.Date(mallReq$date))

#Add Additional Date Fields to Find Location Table
locReqShop$date <- as.Date(locReqShop$timestamp)
locReqShop$day  <- locReqShop$date - min(locReqShop$date)
locReqShop$hour <- substr(locReqShop$timestamp, start = 12, stop = 13)
locReqShop$day_hour <- paste0(locReqShop$day,'_',locReqShop$hour )
locReqShop$day_name <- weekdays(as.Date(locReqShop$date))

#Add Install Dates to Mall Request Table
sessionInstallDates <- session[,c("session_id", "timestamp")]
colnames(sessionInstallDates) <- c("session_id", "installDate")
mallReq <- merge(x=mallReq, y=sessionInstallDates, by="session_id", all.x=TRUE)
mallReq$daysSinceInstall <- as.Date(mallReq$timestamp) - as.Date(mallReq$installDate)

#Add Unique Identifier for Devices
session$macMobileId <- paste0(session$mac,"_" , session$mobile_id)