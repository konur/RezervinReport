###############################################################
#FIND ZERO/NON-ZERO COUNTS
###############################################################
session$mobileIDType <- "Non-Zero"
session$mobileIDType[substr(session$mobile_id, 1, 8)=="01.7.22" | substr(session$mobile_id, 1, 8)=="01.8.0" | substr(session$mobile_id, 1, 8)=="01.8.1"] <- "Zero"
#session$mobileIDType[substr(session$mobile_id, 1, 8)!="00000000"] <- "Non-Zero"

macToggles <- session %>%
  group_by(mac, mobileIDType) %>%
  summarize(macToggleCount = n())

write.csv(macToggles, "mobileIDToggles.csv")

###############################################################
#Last session table
###############################################################
#Session Range: 16 DEC - 4 JAN
sessionSingleVersion <- filter(session, timestamp < "2017-01-05 00:00:00")

# sessionDateGroups <- sessionSingleVersion %>%
#   group_by(mac, timestamp) %>%
#   summarize(count = n())

lastSession <- sessionSingleVersion %>%
  group_by(mac) %>%
  summarize(timestamp = max(timestamp))

lastSessionMerge <- merge(x=sessionSingleVersion, y=lastSession, by=c("mac", "timestamp"), all.y=TRUE)

sessionTimes <- lastSessionMerge %>%
  group_by(mac) %>%
  summarize(count = n())

#Filter table1 and table2
table1 <- filter(lastSessionMerge[,c("mac", "session_id", "timestamp", "mobile_id")], mac %in% sessionTimes$mac[sessionTimes$count==1])
table2 <- filter(lastSessionMerge[,c("mac", "session_id", "timestamp", "mobile_id")], mac %in% sessionTimes$mac[sessionTimes$count>1])

#Macs/Sessions with one last timestamp (clear winner cases)
mallReqTable1 <- filter(mallReq, session_id %in% table1$session_id)
mallReqTable1 <- mallReqTable1 %>%
  group_by(session_id) %>%
  summarize(maxTimestamp = max(timestamp))

table1 <- merge(x=table1, y=mallReqTable1, by="session_id", all.x=TRUE)

#Macs/Sessions with more than one last timestamp (ambiguity cases)
mallReqTable2 <- filter(mallReq, session_id %in% table2$session_id)
mallReqTable2 <- mallReqTable2 %>%
  group_by(session_id) %>%
  summarize(maxTimestamp = max(timestamp))

table2 <- merge(x=table2, y=mallReqTable2, by="session_id", all.x=TRUE)

write.csv(table1, "table1.csv")
write.csv(table2, "table2.csv")