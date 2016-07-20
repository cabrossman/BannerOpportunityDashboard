OAS_catSalesGroup <- function(df){
  
  df$DDM <- ifelse(grepl("^ddm",tolower(df$Advertiser)),1,0)
  df$Passback <- ifelse(grepl("passback",tolower(df$Campaign)),1,0)
  df$Default <- ifelse(grepl("default",tolower(df$Campaign)),1,0)
  df$House <- ifelse(grepl("00001111",tolower(df$Advertiser)),1,0)
  df$salesgroup <- ifelse(df$House > 0,"House",ifelse(df$Default > 0, "unknwon_default",ifelse(df$Passback >0, "Passback",ifelse(df$DDM > 0, "DDM", "Marine"))))
  

                           
 #newly built to capture default data by geo                          
 newDefault <- c('BOALL-ROS-1x1-HOUSE-Bottom', 'BOALL-ROS-1x1-HOUSE-Middle', 'BOALL-ROS-1x1-HOUSE-Middle1', 'BOALL-ROS-1x1-HOUSE-Middle2', 'BOALL-ROS-1x1-HOUSE-Position1', 'BOALL-ROS-1x1-HOUSE-Position2', 'BOALL-ROS-1x1-HOUSE-Right1', 'BOALL-ROS-1x1-HOUSE-Right2', 'BOALL-ROS-1x1-HOUSE-Right3', 'BOALL-ROS-1x1-HOUSE-Top', 'BOALL-ROS-1x1-HOUSE-Top1', 'BOALL-ROS-1x1-HOUSE-x01', 'BOALL-ROS-1x1-HOUSE-x02', 'BOALL-ROS-1x1-HOUSE-x03', 'BOALL-ROS-1x1-HOUSE-x04', 'BOALL-ROS-1x1-HOUSE-x21', 'BOALL-ROS-1x1-HOUSE-x22', 'BOALL-ROS-1x1-HOUSE-x23', 'BOALL-ROS-1x1-HOUSE-x24', 'BOALL-ROS-1x1-HOUSE-x25', 'BOALL-ROS-1x1-HOUSE-x55', 'BOALL-ROS-1x1-HOUSE-x60', 'BOALL-ROS-1x1-HOUSE-x61', 'BTOLALL-ROS-1x1-HOUSE-Bottom1', 'BTOLall-ROS-1x1-HOUSE-Middle', 'BTOLALL-ROS-1x1-HOUSE-Middle1', 'BTOLALL-ROS-1x1-HOUSE-Middle2', 'BTOLALL-ROS-1x1-HOUSE-Middle3', 'BTOLALL-ROS-1x1-HOUSE-Position1', 'BTOLALL-ROS-1x1-HOUSE-Right1', 'BTOLALL-ROS-1x1-HOUSE-Right2', 'BTOLALL-ROS-1x1-HOUSE-Right3', 'BTOLALL-ROS-1x1-HOUSE-Top', 'BTOLALL-ROS-1x1-HOUSE-Top1', 'BTOLALL-ROS-1x1-HOUSE-Top3', 'BTOLALL-ROS-1x1-HOUSE-TopLeft', 'BTOLall-ROS-1x1-HOUSE-TopRight', 'BTOLALL-ROS-1x1-HOUSE-x01', 'BTOLALL-ROS-1x1-HOUSE-x02', 'BTOLALL-ROS-1x1-HOUSE-x03', 'BTOLALL-ROS-1x1-HOUSE-x04', 'BTOLALL-ROS-1x1-HOUSE-x05', 'BTOLALL-ROS-1x1-HOUSE-x06', 'BTOLALL-ROS-1x1-HOUSE-x21', 'BTOLALL-ROS-1x1-HOUSE-x22', 'BTOLALL-ROS-1x1-HOUSE-x23', 'BTOLALL-ROS-1x1-HOUSE-x24', 'BTOLALL-ROS-1x1-HOUSE-x25', 'BTOLALL-ROS-1x1-HOUSE-x30', 'BTOLALL-ROS-1x1-HOUSE-x31', 'BTOLALL-ROS-1x1-HOUSE-x55', 'BTOLALL-ROS-1x1-HOUSE-x56', 'BTOLALL-ROS-1x1-HOUSE-x60', 'BTOLALL-ROS-1x1-HOUSE-x61', 'BTOLALL-ROS-1x1-HOUSE-x63', 'YWALL-ROS-1x1-HOUSE-Bottom', 'YWALL-ROS-1x1-HOUSE-Middle1', 'YWALL-ROS-1x1-HOUSE-Position2', 'YWALL-ROS-1x1-HOUSE-Right1', 'YWALL-ROS-1x1-HOUSE-Right2', 'YWALL-ROS-1x1-HOUSE-Top', 'YWall-ROS-1x1-HOUSE-Top1', 'YWALL-ROS-1x1-HOUSE-x01', 'YWALL-ROS-1x1-HOUSE-x02', 'YWALL-ROS-1x1-HOUSE-x03', 'YWALL-ROS-1x1-HOUSE-x04', 'YWALL-ROS-1x1-HOUSE-x21', 'YWALL-ROS-1x1-HOUSE-x22', 'YWALL-ROS-1x1-HOUSE-x23', 'YWALL-ROS-1x1-HOUSE-x24', 'YWALL-ROS-1x1-HOUSE-x25', 'YWALL-ROS-1x1-HOUSE-x30')
 df$salesgroup <- ifelse(df$Campaign %in% newDefault,"known_default",df$salesgroup)
 
 return(df$salesgroup)
}



