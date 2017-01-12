#Notes: locReqShop colnames fixed

SQLusername <- "konur"
SQLpassword <- "ihg345%,;6kK"
SQLhost     <- "rezervin-master.malliq-api.com"

###############################################################
#Load Session Data
###############################################################
if(exists("session") == FALSE){
  print("...Downloading Session Table...")

  malliq = dbConnect(MySQL(), user = SQLusername, password = SQLpassword, dbname='malliq', host="rezervin-master.malliq-api.com")
  malliq_persistent = dbConnect(MySQL(), user = SQLusername, password = SQLpassword, dbname='malliq_persistent', host="rezervin-master.malliq-api.com")

  session <- dbSendQuery(malliq, "SELECT * FROM malliq.session S WHERE S.timestamp > '2016-12-16 14:00:00'") #Rezervin launch = 16 dec 2016 14:00
  session <- fetch(session, n = -1)

  print("...Done...")
}else
  print("...Session Table (session): Already Exists and Not Loaded Again...")

###############################################################
#Load Find Mall Request Data (after 2016-12-15)
###############################################################
if(exists("mallReq") == FALSE){
  print("...Downloading Mall Request Table...")

  malliq = dbConnect(MySQL(), user=SQLusername, password=SQLpassword, dbname='malliq', host="rezervin-master.malliq-api.com")
  malliq_persistent = dbConnect(MySQL(), user=SQLusername, password=SQLpassword, dbname='malliq_persistent', host="rezervin-master.malliq-api.com")

  mallReq <- dbSendQuery(malliq_persistent, "SELECT * FROM malliq_persistent.find_mall_request S WHERE DATE(S.timestamp) > DATE('2016-12-15')") #Rezervin launch = 16 dec 2016
  mallReq <- fetch(mallReq, n = -1)

  print("...Done...")
}else
  print("...Find Mall Request Table (mallReq): Already Exists and Not Loaded Again...")

###############################################################
#Load Find Location Request Data (after 2016-12-26 11:00:00 TSI)
###############################################################
if(exists("locReqShop") == FALSE){
  print("...Downloading Find Location Request Table...")

  malliq = dbConnect(MySQL(), user = SQLusername, password = SQLpassword, dbname='malliq', host="rezervin-master.malliq-api.com")
  malliq_persistent = dbConnect(MySQL(), user = SQLusername, password = SQLpassword, dbname='malliq_persistent', host="rezervin-master.malliq-api.com")

  sqlQuery <- "SELECT *, ASTEXT(geom) AS astextGeom
  FROM malliq_persistent.find_location_request FLR
  LEFT JOIN malliq.mall_shop MS ON FLR.mall_id = MS.mall_id AND FLR.floor_id = MS.floor_id AND  ST_CONTAINS(MS.geom, POINT(FLR.x, FLR.y))
  WHERE FLR.timestamp > '2016-12-26 11:00:00'"

  locReqShop <- dbSendQuery(malliq_persistent, sqlQuery)
  locReqShop <- fetch(locReqShop, n = -1)

  #change column names to avoid duplicates
  colnames(locReqShop) <- c("id","session_id","mall_id","floor_id","lat","lng","x","y","timestamp","is_processed","match_ratio",
                            "url","chr","ax","ay","az","mx","my","mz","altitude","pressure","baro","horizontal_accuracy","wake_up_reason","status",
                            "location_engine_time", "mall_id2", "shop_id","floor_id2","geom","astextGeom")

  print("...Done...")
}else
  print("...Find Location Request Table (locReqShop): Already Exists and Not Loaded Again...")

###############################################################
#Load Mall List
###############################################################
if(exists("mallList") == FALSE){
  print("...Downloading Mall Table...")

  malliq = dbConnect(MySQL(), user=SQLusername, password = SQLpassword, dbname='malliq', host="rezervin-master.malliq-api.com")
  malliq_persistent = dbConnect(MySQL(), user=SQLusername, password=SQLpassword, dbname = 'malliq_persistent', host="rezervin-master.malliq-api.com")

  mallList <- dbSendQuery(malliq, "SELECT * FROM malliq.mall")
  mallList <- fetch(mallList, n = -1)

  print("...Done...")
}else
  print("...Mall List Table (mallList): Already Exists and Not Loaded Again...")

###############################################################
#Load Shop List
###############################################################
if(exists("shopList") == FALSE){
  print("...Downloading Shop Table...")

  malliq = dbConnect(MySQL(), user=SQLusername, password = SQLpassword, dbname='malliq', host="rezervin-master.malliq-api.com")
  malliq_persistent = dbConnect(MySQL(), user=SQLusername, password=SQLpassword, dbname = 'malliq_persistent', host="rezervin-master.malliq-api.com")

  shopList <- dbSendQuery(malliq, "SELECT * FROM malliq.shop")
  shopList <- fetch(shopList, n = -1)

  print("...Done...")
}else
  print("...Shop List Table (shopList): Already Exists and Not Loaded Again...")




print("Loading SQL Data Is Complete!")
