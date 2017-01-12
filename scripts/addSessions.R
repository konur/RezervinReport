#This function receives the data and provides session info to identify visit/pass by sessions
#----------------------------------------------------------------------------------------------
#Required data columns: session_id, mall_id, timestamp
#----------------------------------------------------------------------------------------------
#Parameters: 
#maxMinsSameMall -> max amount of minutes needed in order for two consecutive requests to count as the same session

addSessions <- function(mallRestReq, maxMinsSameMall, mode = c("mall", "shop")) {

  #Filter & Sort Data
  mallRestReq <- mallRestReq %>% 
    filter(mall_id != 0 & is.na(mall_id) == FALSE) %>%
    arrange(desc(session_id), timestamp)
  
  #Identify Mall Sessions
  if (mode == "mall"){
    print("...Finding Mall Sessions...")
    
    sessionNum <- 1
    mallRestReq$sessionNo <- 0
    mallRestReq$sessionNo[[1]] <- 1
    prevUser <- as.character(mallRestReq$session_id[[1]])
    prevDate <- as.character(mallRestReq$timestamp[[1]])
    prevMallid <- as.character(mallRestReq$mall_id[[1]])
    
    foreach (i = 2:nrow(mallRestReq)) %do% {
      curUser <- as.character(mallRestReq$session_id[[i]])
      curDate <- as.character(mallRestReq$timestamp[[i]])
      curMallid <- as.character(mallRestReq$mall_id[[i]])
      
      date_diff <- as.numeric(difftime(strptime(curDate, "%Y-%m-%d %H:%M:%S"), strptime(prevDate, "%Y-%m-%d %H:%M:%S"), units="mins"))
      
      if (curUser==prevUser & curMallid==prevMallid & abs(date_diff)<=maxMinsSameMall){
        mallRestReq$sessionNo[[i]] <- sessionNum
      }
      else{
        sessionNum <- sessionNum + 1
        mallRestReq$sessionNo[[i]] <- sessionNum
      }
      
      prevUser <- curUser
      prevDate <- curDate
      prevMallid <- curMallid
    }
  } else if(mode == "shop"){
    print("...Finding Shop Sessions...")
    
    sessionNum <- 1
    mallRestReq$sessionNo <- 0
    mallRestReq$sessionNo[[1]] <- 1
    prevUser <- as.character(mallRestReq$session_id[[1]])
    prevDate <- as.character(mallRestReq$timestamp[[1]])
    prevMallid <- as.character(mallRestReq$mall_id[[1]])
    prevShopid <- as.character(mallRestReq$shop_id[[1]])
    
    foreach (i = 2:nrow(mallRestReq)) %do% {
      curUser <- as.character(mallRestReq$session_id[[i]])
      curDate <- as.character(mallRestReq$timestamp[[i]])
      curMallid <- as.character(mallRestReq$mall_id[[i]])
      curShopid <- as.character(mallRestReq$shop_id[[i]])
      
      date_diff <- as.numeric(difftime(strptime(curDate, "%Y-%m-%d %H:%M:%S"), strptime(prevDate, "%Y-%m-%d %H:%M:%S"), units="mins"))
      
      if (curUser==prevUser & curMallid==prevMallid & curShopid==prevShopid & abs(date_diff)<=maxMinsSameMall){
        mallRestReq$sessionNo[[i]] <- sessionNum
      }
      else{
        sessionNum <- sessionNum + 1
        mallRestReq$sessionNo[[i]] <- sessionNum
      }
      
      prevUser <- curUser
      prevDate <- curDate
      prevMallid <- curMallid
      prevShopid <- curShopid
    }
  }
  

  return(mallRestReq)
}