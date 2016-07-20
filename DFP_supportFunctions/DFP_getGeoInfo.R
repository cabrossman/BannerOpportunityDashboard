DFP_getGeoInfo <- function(){
  request_data <- list(selectStatement=
                         list(query=paste('select Id, Name,', 
                                          'CanonicalParentId, CountryCode,',
                                          "Type from Geo_Target")))
  dfp_select_result <- dfp_select(request_data)$rval
  final_result <- dfp_select_parse(dfp_select_result)
  return(final_result)
  
}





