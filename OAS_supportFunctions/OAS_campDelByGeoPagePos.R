OAS_campDelByGeoPagePos <- function(campList = NULL,salesgroup = NULL,advertiser = NULL,geo = 'state/province delivery information',start_date = as.character(Sys.Date() - 31), end_date = as.character(Sys.Date() - 1)){
    
    my_credentials <- oasAuth()
    
    if(is.null(campList) | is.null(salesgroup) | is.null(advertiser)){
      warning("one of campList, salesgroup, or advertiser is null")
      warning("using auto defaults of all campaigns in last 30 days")
      campsBySite <- OAS_listCampPerSites()
      campList = campsBySite$Campaign
      salesgroup = campsBySite$salesgroup
      advertiser = campsBySite$Advertiser
    }
    
    print("getting all of the campaign details - this may take over an hour")
    DelByGeo <- NULL; DelByPagePos <- NULL
    for(i in 1:length(campList)){
      print(paste0(i," of ",length(campList)))
      
      result <- tryCatch({
        tempGeo <- oas_report(credentials = my_credentials, 
                              report_type = 'campaign delivery', 
                              report_name = geo, 
                              id = campList[i], 
                              start_date=start_date, 
                              end_date=end_date)
        
        tempPagePos <- oas_report(credentials = my_credentials, 
                                  report_type = 'campaign delivery', 
                                  report_name = 'page@position delivery information', 
                                  id = campList[i], 
                                  start_date=start_date, 
                                  end_date=end_date)
        
        if(nrow(tempGeo) > 0){
          tempGeo[,2] <- ifelse(tempGeo[,2] %in% c('Unknown', 'UNKNOWN'),'unknown',tempGeo[,2])
          tempGeo <- cbind.data.frame(Advertiser = advertiser[i],
                                      campaign = campList[i],
                                      #business = campsBySite$business[i], 
                                      salesgroup = salesgroup[i], 
                                      #TotCampImp = campsBySite$Impressions[i], 
                                      Geo = tempGeo[,2],
                                      impByGeo = as.numeric(tempGeo$Imps))
          
          DelByGeo <- rbind(DelByGeo,tempGeo)
        }
        
        if(nrow(tempPagePos) > 0){
          tempPagePos <- cbind.data.frame(Advertiser = advertiser[i],
                                          campaign = campList[i],
                                          #business = campsBySite$business[i], 
                                          salesgroup = salesgroup[i], 
                                          #TotCampImp = campsBySite$Impressions[i], 
                                          Page = tempPagePos$Page,
                                          Pos = tempPagePos$Position,
                                          impByPagePos = as.numeric(tempPagePos$Impressions))
          
          DelByPagePos <- rbind(DelByPagePos,tempPagePos)
        }
        
        
      }, error = function(err) {
        
        # error handler picks up where error was generated
        print(paste("MY_ERROR:  ",err))
        #next
        
      }) 
      
      
    }
    rm(result,tempGeo,tempPagePos)
    del <- list(DelByPagePos,DelByGeo)
    return(del)
}