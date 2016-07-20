library(stringr)
library(dplyr)

#functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

stringToRegex <- function (string,delim) {
  string <- tolower(trim(unlist(strsplit(string,delim))))
  string <- paste0('^',string,'$')
  string <- paste(string, collapse = "|")
  return (string)
}

factorPV <- function (regex_brand,regex_class,portalAbbv,data) {
  portNams <- c("YachtWorld","Boat Trader","boats.com")
  p2 <- c("YW","BT","BC")
  portals <- cbind.data.frame(portNams,p2)
  portals <- portals %>% filter(p2 %in% portalAbbv)
  data <- data %>% filter(portal %in% portals$portNams)
  totPV <- sum(data$pv)
  
  data <- data %>% filter(grepl(regex_brand,tolower(data$make))|grepl(regex_class,tolower(data$class)))
  filterPV <- sum(data$pv)
  return(filterPV/totPV)
}

factorPV_Type <- function (regex_brand,portalAbbv,data,regex_type) {
  portNams <- c("YachtWorld","Boat Trader","boats.com")
  p2 <- c("YW","BT","BC")
  portals <- cbind.data.frame(portNams,p2)
  portals <- portals %>% filter(p2 %in% portalAbbv)
  data <- data %>% filter(portal %in% portals$portNams)
  totPV <- sum(data$pv)
  
  data <- data %>% filter(grepl(regex_brand,tolower(data$make))|grepl(regex_type,tolower(data$type)))
  filterPV <- sum(data$pv)
  return(filterPV/totPV)
}

factorPV_classType <- function (regex_brand,regex_class,portalAbbv,data,regex_type) {
  portNams <- c("YachtWorld","Boat Trader","boats.com")
  p2 <- c("YW","BT","BC")
  portals <- cbind.data.frame(portNams,p2)
  portals <- portals %>% filter(p2 %in% portalAbbv)
  data <- data %>% filter(portal %in% portals$portNams)
  totPV <- sum(data$pv)
  
  data2 <- data %>% filter(grepl(regex_brand,tolower(data$make)))
  data3 <- data %>% filter(grepl(regex_type,tolower(data$type)),grepl(regex_class,tolower(data$class)))
  data4 <- data %>% filter(grepl(regex_brand,tolower(data$make)),grepl(regex_type,tolower(data$type)),grepl(regex_class,tolower(data$class)))
  filterPV <- sum(data2$pv) + sum(data3$pv) - sum(data4$pv)
  return(filterPV/totPV)
}

