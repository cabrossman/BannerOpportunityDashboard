
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
      
      # temp$DDM <- ifelse(grepl("^ddm",tolower(temp$Advertiser)),1,0)
      # temp$Passback <- ifelse(grepl("passback",tolower(temp$Campaign)),1,0)
      # temp$Default <- ifelse(grepl("default",tolower(temp$Campaign)),1,0)
      # temp$House <- ifelse(grepl("00001111",tolower(temp$Advertiser)),1,0)
      # temp$salesgroup <- ifelse(temp$House > 0,"House",ifelse(temp$Default > 0, "Default",ifelse(temp$Passback >0, "Passback",ifelse(temp$DDM > 0, "DDM", "Marine"))))
      # 
      temp <- cbind.data.frame(business = business,
                               website = sitesId[i],
                               Advertiser = temp$Advertiser,
                               Campaign = temp$Campaign,  
                               # salesgroup = temp$salesgroup, 
                               Impressions = as.numeric(temp$Impressions)
      )
      
      campsBySite <- rbind(campsBySite, temp)
    }
    rm(temp)
    
  }
  
  campsBySite$salesgroup <- OAS_catSalesGroup(campsBySite)
  
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
  
  
  # newDefault <- c('BOALL-ROS-1x1-HOUSE-Bottom', 'BOALL-ROS-1x1-HOUSE-Middle', 'BOALL-ROS-1x1-HOUSE-Middle1', 'BOALL-ROS-1x1-HOUSE-Middle2', 'BOALL-ROS-1x1-HOUSE-Position1', 'BOALL-ROS-1x1-HOUSE-Position2', 'BOALL-ROS-1x1-HOUSE-Right1', 'BOALL-ROS-1x1-HOUSE-Right2', 'BOALL-ROS-1x1-HOUSE-Right3', 'BOALL-ROS-1x1-HOUSE-Top', 'BOALL-ROS-1x1-HOUSE-Top1', 'BOALL-ROS-1x1-HOUSE-x01', 'BOALL-ROS-1x1-HOUSE-x02', 'BOALL-ROS-1x1-HOUSE-x03', 'BOALL-ROS-1x1-HOUSE-x04', 'BOALL-ROS-1x1-HOUSE-x21', 'BOALL-ROS-1x1-HOUSE-x22', 'BOALL-ROS-1x1-HOUSE-x23', 'BOALL-ROS-1x1-HOUSE-x24', 'BOALL-ROS-1x1-HOUSE-x25', 'BOALL-ROS-1x1-HOUSE-x55', 'BOALL-ROS-1x1-HOUSE-x60', 'BOALL-ROS-1x1-HOUSE-x61', 'BTOLALL-ROS-1x1-HOUSE-Bottom1', 'BTOLall-ROS-1x1-HOUSE-Middle', 'BTOLALL-ROS-1x1-HOUSE-Middle1', 'BTOLALL-ROS-1x1-HOUSE-Middle2', 'BTOLALL-ROS-1x1-HOUSE-Middle3', 'BTOLALL-ROS-1x1-HOUSE-Position1', 'BTOLALL-ROS-1x1-HOUSE-Right1', 'BTOLALL-ROS-1x1-HOUSE-Right2', 'BTOLALL-ROS-1x1-HOUSE-Right3', 'BTOLALL-ROS-1x1-HOUSE-Top', 'BTOLALL-ROS-1x1-HOUSE-Top1', 'BTOLALL-ROS-1x1-HOUSE-Top3', 'BTOLALL-ROS-1x1-HOUSE-TopLeft', 'BTOLall-ROS-1x1-HOUSE-TopRight', 'BTOLALL-ROS-1x1-HOUSE-x01', 'BTOLALL-ROS-1x1-HOUSE-x02', 'BTOLALL-ROS-1x1-HOUSE-x03', 'BTOLALL-ROS-1x1-HOUSE-x04', 'BTOLALL-ROS-1x1-HOUSE-x05', 'BTOLALL-ROS-1x1-HOUSE-x06', 'BTOLALL-ROS-1x1-HOUSE-x21', 'BTOLALL-ROS-1x1-HOUSE-x22', 'BTOLALL-ROS-1x1-HOUSE-x23', 'BTOLALL-ROS-1x1-HOUSE-x24', 'BTOLALL-ROS-1x1-HOUSE-x25', 'BTOLALL-ROS-1x1-HOUSE-x30', 'BTOLALL-ROS-1x1-HOUSE-x31', 'BTOLALL-ROS-1x1-HOUSE-x55', 'BTOLALL-ROS-1x1-HOUSE-x56', 'BTOLALL-ROS-1x1-HOUSE-x60', 'BTOLALL-ROS-1x1-HOUSE-x61', 'BTOLALL-ROS-1x1-HOUSE-x63', 'YWALL-ROS-1x1-HOUSE-Bottom', 'YWALL-ROS-1x1-HOUSE-Middle1', 'YWALL-ROS-1x1-HOUSE-Position2', 'YWALL-ROS-1x1-HOUSE-Right1', 'YWALL-ROS-1x1-HOUSE-Right2', 'YWALL-ROS-1x1-HOUSE-Top', 'YWall-ROS-1x1-HOUSE-Top1', 'YWALL-ROS-1x1-HOUSE-x01', 'YWALL-ROS-1x1-HOUSE-x02', 'YWALL-ROS-1x1-HOUSE-x03', 'YWALL-ROS-1x1-HOUSE-x04', 'YWALL-ROS-1x1-HOUSE-x21', 'YWALL-ROS-1x1-HOUSE-x22', 'YWALL-ROS-1x1-HOUSE-x23', 'YWALL-ROS-1x1-HOUSE-x24', 'YWALL-ROS-1x1-HOUSE-x25', 'YWALL-ROS-1x1-HOUSE-x30')
  # campsBySite_small$salesgroup <- ifelse(campsBySite_small$Campaign %in% newDefault,"Default",campsBySite_small$salesgroup)
  
  
  return(campsBySite_small)
}