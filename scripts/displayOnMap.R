displayOnMap <- function (mallReq){
  ###############################################################
  #Mall Requests Displayed on Map
  ###############################################################
  # #Add Near Mall Indicator
  # mallReq$nearMall <- FALSE
  #
  # mallReq$nearMall[mallReq$mall_id > 0 & mallReq$mall_id < max_mallid] <- TRUE
  #
  #
  # if(exists("turkey.map") == FALSE){
  #   turkey.map <- get_map("Turkey", zoom=5)
  #   ist.map <- get_map("Istanbul", zoom=9)
  #   ank.map <- get_map("Ankara", zoom=10)
  #   izmir.map <- get_map("Izmir", zoom=10)
  # }
  # #Points on Map
  # ggmap(turkey.map) + ggtitle("Turkey: Rezervin Location Data (Find Mall Requests)") +
  #   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) +
  #   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
  #
  # ggmap(ist.map) + ggtitle("Istanbul: Rezervin Location Data (Find Mall Requests)") +
  #   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) +
  #   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
  #
  # ggmap(ank.map) + ggtitle("Ankara: Rezervin Location Data (Find Mall Requests)") +
  #   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) +
  #   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
  #
  # ggmap(izmir.map) + ggtitle("Izmir: Rezervin Location Data (Find Mall Requests)") +
  #   geom_point(data=mallReq, aes(x=lng, y=lat, fill=nearMall, color=nearMall), shape = 21, size=1.1) +
  #   scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1")
  #
  # #Heat Map
  # ggmap(ist.map) + ggtitle("Heat Map: Istanbul") +
  #   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")
  #
  # ggmap(ank.map) + ggtitle("Heat Map: Ankara") +
  #   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")
  #
  # ggmap(izmir.map) + ggtitle("Heat Map: Izmir") +
  #   stat_density2d(data=mallReq, aes(x=lng, y=lat, fill=..level.., alpha=..level..),size=2, geom="polygon")
}