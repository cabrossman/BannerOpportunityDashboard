
OAS_listCampPerSites <- function(sitesId = NULL, start_date = as.character(Sys.Date() - 31), end_date = as.character(Sys.Date() - 1)){
    #Get all campaigns for each site, append data on website (YW,BT,BC) and categorize advertiser (DDM, PASSBACK,Default,House, Marine)
    
  
    my_credentials <- oasAuth()
    if(is.null(sitesId)){
      sitesId <- OAS_listMarineSites()$Id
    }
  
    print("getting list of campaigns for each site")
    campsBySite <- NULL
    for(i in 1:length(sitesId)){
      print(paste0(i," of ", length(sitesId)))
      temp <- oas_report(credentials = my_credentials, 
                         report_type = 'site delivery', 
                         report_name = 'campaign by advertiser report', 
                         id = sitesId[i], 
                         start_date=start_date, 
                         end_date=end_date )
      
      
      
      
      
      if(nrow(temp) > 0){
        
        if(grepl("boats",sitesId[i])){
          business <- "BC"
        } else if(grepl("yacht",sitesId[i])){
          business <- "YW"
        } else{
          business <- "BT"
        }
        
        temp$DDM <- ifelse(grepl("^ddm",tolower(temp$Advertiser)),1,0)
        temp$Passback <- ifelse(grepl("passback",tolower(temp$Campaign)),1,0)
        temp$Default <- ifelse(grepl("default",tolower(temp$Campaign)),1,0)
        temp$House <- ifelse(grepl("00001111",tolower(temp$Advertiser)),1,0)
        temp$salesgroup <- ifelse(temp$House > 0,"House",ifelse(temp$Default > 0, "Default",ifelse(temp$Passback >0, "Passback",ifelse(temp$DDM > 0, "DDM", "Marine"))))
        
        temp <- cbind.data.frame(business = business,
                                 website = sitesId[i],
                                 Advertiser = temp$Advertiser,
                                 Campaign = temp$Campaign,  
                                 salesgroup = temp$salesgroup, 
                                 Impressions = as.numeric(temp$Impressions)
        )
        
        campsBySite <- rbind(campsBySite, temp)
      }
      rm(temp)
      
    }
    
    campsBySite_small <- campsBySite %>% 
      group_by( Campaign, Advertiser, salesgroup) %>% 
      summarise(Imp = sum(as.numeric(Impressions)), BCimps = sum(ifelse(business == 'BC',as.numeric(Impressions),0)),
                YWimps = sum(ifelse(business == 'YW',as.numeric(Impressions),0)), BTimps = sum(ifelse(business == 'BT',as.numeric(Impressions),0)),
                BC = sum(business == 'BC'), YW = sum(business == 'YW'), BT = sum(business == 'BT')) %>% mutate(NumOfSite = BC + YW + BT) %>% 
      ungroup() %>% 
      arrange(desc(NumOfSite)) %>% 
      mutate(MultiSite = !(BC/NumOfSite == 1 | YW/NumOfSite ==1 | BT/NumOfSite == 1)) %>%
      mutate(BCpct = BCimps/Imp, YWpct = YWimps/Imp, BTpct = BTimps/Imp, BCavgImp = ifelse(BC > 0,BCimps/BC,0), 
             YWavgImp = ifelse(YW > 0,YWimps/YW,0), BTavgImp = ifelse(BT>0,BTimps/BT,0))
    
    return(campsBySite_small)
}
