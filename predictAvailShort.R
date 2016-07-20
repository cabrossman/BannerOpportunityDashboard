#Get all the necessary functions
path <- "C:\\Users\\christopher.brossman\\Documents\\R files\\R_Functions\\AuthFunctions.R"
source(path)


dir.create("output")
path <- paste0(getwd(),"/output/","")
end_date <- as.character(Sys.Date() - 1)
start_date <- as.character(Sys.Date()-8)

#credentials to OAS
my_credentials <- oasAuth()
#get list of campaigns
camps <- OAS_listCampPerSites()
#filter for just DDM campaigns
camps <- camps[camps$salesgroup == 'DDM',]
#get delivery by page@pos and Geo
listOfDel <- OAS_campDelByGeoPagePos(camps$Campaign,camps$salesgroup,camps$Advertiser, start_date = start_date, end_date = end_date)
#separate list into two individual files
DelByPagePos <- listOfDel[[1]]
DelByGeo <- listOfDel[[2]]
#categorize the geo
DelByGeo <- OAS_catGeo(DelByGeo)
#categorize the page/pos
DelByPagePos$section <- OAS_catPageSection(DelByPagePos$Page)
DelByPagePos$business <- OAS_websiteCat(DelByPagePos$Page)
DelByPagePos$fullDomain <- OAS_catFullDomainName(DelByPagePos$Page)


DelByPagePosSectionGeo <- OAS_joinGeo_PagePos(DelByPagePos,DelByGeo)

DelByPagePosSectionGeo <- DelByPagePosSectionGeo %>% mutate(imps = EstImpByPosSecGeo*(365/84)*.85,international = ifelse(Country %in% c('US','CA'), 0,1), date = end_date) %>% mutate(key = paste0("OAS_",business,campaign,Geo,Pos,section,date))
DelByPagePosSectionGeo <- DelByPagePosSectionGeo %>% select(key, date, business,salesgroup = salesgroup.x,advertiser = Advertiser,campaign,geo = Geo,country = Country,state = State,area = Area, international,pos = Pos, section, imps)
DelByPagePosSectionGeo <- DelByPagePosSectionGeo[,c("key","date","business","salesgroup","advertiser","campaign","geo","country","state","area","international","pos","section","imps")]
# DelByPagePosGeo <- DelByPagePosGeo %>% mutate(EstImpByPosGeo_Mon = EstImpByPosGeo*(365/84)) %>%  select(business,salesgroup = salesgroup.x,Advertiser,campaign,Geo,Country,State,Area,Pos, EstImpByPosGeo_Mon)

#saveFiles&uploadToGooglDrive
print("Writing files to drive")
nam <- paste0(path,"predictAvail_",end_date,".csv")
write.csv(DelByPagePosSectionGeo,nam,row.names=FALSE)



