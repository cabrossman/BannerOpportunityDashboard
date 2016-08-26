Oper_catTargeting <- function(mydata){



  #when l.Targeting_Name like in (use numbers here) and len(l.Targeting_Option_Name) - len(replace(l. Targeting_Option_Name,'-','')) > 1 then "(?<=-).*"
  #when l.Targeting_Name like in (use numbers here) and len(l.Targeting_Option_Name) - len(replace(l. Targeting_Option_Name,'-','')) = 1 then "(?<=-)(.*?)(?=-)"
  #when l.Targeting_Name like in (use numbers here #2) then l.Targeting_Option_Name
  #when l.Targeting_Name like in (use numbers here) then  "(?<=-- ).*"
  #when l.Targeting_Name like in (use numbers here) then  lower(l.targeting_option_name)
  #when l.Targeting_Name like in (use numbers here) and substr(l.targeting_option_name,1,4) = "BTOL" and len(l.Targeting_Option_Name) - len(replace(l. Targeting_Option_Name,'-','')) > 1 then CONCATENATE UPPER("(?<=- ).*" AND "_" AND  rtrim("(?<=-).*")
  #when l.Targeting_Name like in (use numbers here)  and substr(l.targeting_option_name,1,4) = "BTOL" and len(l.Targeting_Option_Name) - len(replace(l. Targeting_Option_Name,'-','')) = 1 then rtrim("(?<=-)(.*?)(?=-)")
  #when l.Targeting_Name like in (use numbers here) and substr(l.targeting_option_name,1,4) <> "BTOL" and charindex(" ",l.Targeting_option_name) <> 0 then CONCATENATE UPPER("(?<=-)(.*?)(?= )" AND "_" AND  rtrim("(?<= ).*")
#when l.Targeting_Name like in (use numbers here) and substr(l.targeting_option_name,1,4) <> "BTOL" and charindex(" ",l.Targeting_option_name) = 0 then UPPER("(?<=-)(.*?)(?= )" 

  library(stringr)
  # mydata <- read.csv("targeting.csv",stringsAsFactors = FALSE)
  options(stringsAsFactors = FALSE)
  
  vector <- mydata$Targeting_Option_Name
  vector <- NA
  
  for(i in 1:NROW(mydata$Targeting_Name)){
    
    if(mydata$Targeting_Name[i] %in% c('BRAND-MAKE-EXCLUDE-search-nia','BRAND-MAKE-search-ia','BRAND-MAKE-search-nia') && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub("-","",mydata$Targeting_Option_Name[i])) == 1)){
      NEW_NAME <- tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-).*"))
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
    
    } else if (mydata$Targeting_Name[i] %in% c('BRAND-MAKE-EXCLUDE-search-nia','BRAND-MAKE-search-ia','BRAND-MAKE-search-nia') && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub("-","",mydata$Targeting_Option_Name[i])) > 1)){
      NEW_NAME <- tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-)(.*?)(?=-)"))
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
    } else if (mydata$Targeting_Name[i] %in% c('CONTINENT-IP-ia', 'COUNTRY-IP-ia', 'DFP-City-IP-ia', 'DFP-DMA-IP-ia', 'DMA-IP-EXCLUDE-nia', 'DMA-IP-ia')){
      NEW_NAME <- tolower(mydata$Targeting_Option_Name[i])
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
    } else if (mydata$Targeting_Name[i] %in% c('STATE_PROV-IP-EXCLUDE-nia', 'STATE_PROV-IP-ia')){
      NEW_NAME <-  tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-- ).*"))
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
    } else if (mydata$Targeting_Name[i] %in% c('STATE-EXCLUDE-search-nia', 'STATE-search-nia')){
      NEW_NAME <-  tolower(mydata$Targeting_Option_Name[i])
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
    } else if (mydata$Targeting_Name[i] %in% c('CATEGORY-CLASS-TYPE-EXCLUDE-search-nia', 'CATEGORY-CLASS-TYPE-search-ia', 'CATEGORY-CLASS-TYPE-search-nia') && substr(mydata$Targeting_Option_Name[i],1,4) == 'BTOL' && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub("-","",mydata$Targeting_Option_Name[i])) > 1)){
      
      name1 <- tolower(str_extract(mydata$Targeting_Option_Name[i], "[^-]+$"))
      name2 <- tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-)(.*?)(?=-)"))
      name1 <- gsub("^\\s+|\\s+$", "", name1)
      name2 <- gsub("^\\s+|\\s+$", "", name2)
      if(substr(name1,1,1) == 'c'){
        NEW_NAME <- paste0("c_",name2)
        vector[i] <- NEW_NAME
  
      }
      if(substr(name1,1,1) == 't'){
        NEW_NAME <- paste0("t_",name2)
        vector[i] <- NEW_NAME
  
      }
      
    } else if (mydata$Targeting_Name[i] %in% c('CATEGORY-CLASS-TYPE-EXCLUDE-search-nia', 'CATEGORY-CLASS-TYPE-search-ia', 'CATEGORY-CLASS-TYPE-search-nia') && substr(mydata$Targeting_Option_Name[i],1,4) == 'BTOL' && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub("-","",mydata$Targeting_Option_Name[i])) == 1)){
      NEW_NAME <-  tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-)(.*?)(?= )"))
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
      
    } else if (mydata$Targeting_Name[i] %in% c('CATEGORY-CLASS-TYPE-EXCLUDE-search-nia', 'CATEGORY-CLASS-TYPE-search-ia', 'CATEGORY-CLASS-TYPE-search-nia') && substr(mydata$Targeting_Option_Name[i],1,4) != 'BTOL' && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub(" ","",mydata$Targeting_Option_Name[i])) > 0)){
      name1 <-  tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<=-)(.*?)(?= )"))
      name2 <-  tolower(str_extract(mydata$Targeting_Option_Name[i], "(?<= ).*"))
      name1 <- gsub("^\\s+|\\s+$", "", name1)
      name2 <- gsub("^\\s+|\\s+$", "", name2)
      name1 <- substr(name1,1,1)
      NEW_NAME <- paste0(name1,'_',name2)
      vector[i] <- NEW_NAME
  
    } else if (mydata$Targeting_Name[i] == 'CATEGORY-CLASS-TYPE-search-ia' && substr(mydata$Targeting_Option_Name[i],1,4) != 'BTOL' && (nchar(mydata$Targeting_Option_Name[i]) - nchar(gsub(" ","",mydata$Targeting_Option_Name[i])) == 0)){
      NEW_NAME <-  tolower(str_extract(mydata$Targeting_Option_Name[i], "[^-]+$"))
      NEW_NAME <- gsub("^\\s+|\\s+$", "", NEW_NAME)
      vector[i] <- NEW_NAME
  
    } else {
      vector[i] <- mydata$Targeting_Option_Name[i]
    }
    
  }
  
  return(vector)
}