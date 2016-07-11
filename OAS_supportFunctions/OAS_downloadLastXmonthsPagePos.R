getDelivByFullDomain <- function(monthsago){
  
  
  options(stringsAsFactors = FALSE)
  
  suppressMessages(library(roas))
  suppressMessages(library(XML))
  suppressMessages(library(lubridate))
  suppressMessages(library(stringr))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  
  # these are my credentials currently. I think they will work for marine 
  #credentials_string <- '<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n                <env:Envelope xmlns:n1=\"https://api.oas.tfsm.com/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"><env:Body>\n                <n1:OasXmlRequest xmlns:n1=\"https://api.oas.tfsm.com/\"><String_1>Dominion</String_1>\n                <String_2>MortimerS</String_2>\n                <String_3>mortimer124!</String_3>'
  credentials_string <- oasAuth()
  sites <- OAS_listMarineSites()
  
  
  data <- NULL
  for(i in 1:monthsago){
    print(paste0(i," of ", monthsago))
    start_date <- as.character(floor_date(floor_date(Sys.Date(),"month") - months(i),"month"))
    end_date <- as.character(floor_date(floor_date(Sys.Date(),"month") - months(i-1),"month") - 1)
    
    print(start_date);print(end_date)
    
    bigTemp <- NULL
    for(j in 1:length(sites$Id)){
      print(paste0(j," of ", length(sites$Id)))
      temp <- oas_report(credentials = credentials_string, 
                         report_type = 'site delivery', 
                         report_name = 'url delivery information', 
                         id = sites$Id[j], 
                         start_date=start_date, 
                         end_date=end_date )
      if(nrow(temp)>0){
        
        temp <- cbind.data.frame(site = sites$Id[j], URL = temp$URL,year = substr(start_date,1,4), month = substr(start_date,6,7), imp = as.numeric(temp$Imps), clicks = as.numeric(temp$Clicks))
        
        bigTemp <- rbind(bigTemp,temp)
      }
    }
    
    data <- rbind(data,bigTemp)
  }
  rm(bigTemp,temp)
  return(data)
}
