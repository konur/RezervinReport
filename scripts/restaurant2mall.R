restaurant2mall <- function(mallReq) {
  
  #Replace some restaurants w/ malls
  mallReq$mall_id[mallReq$mall_id==1126 | mallReq$mall_id==1117 | mallReq$mall_id==1089 | mallReq$mall_id==1084 | mallReq$mall_id==1083] <- 27
  mallReq$mall_id[mallReq$mall_id==1026 | mallReq$mall_id==1031 | mallReq$mall_id==1127] <- 26
  
  return(mallReq)
}