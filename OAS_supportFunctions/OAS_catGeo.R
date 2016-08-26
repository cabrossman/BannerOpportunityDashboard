OAS_catGeo <- function(DelByGeo_df){
      
  print("Categorizing Data")
  #This section categorizes the geographic areas based on sales criteria
  #This also categorizes if the webserver was able to identify the geo information and separate the GEO into state and country
  CA_EAST <- c('CA -- Quebec', 'CA -- Prince Edward Island', 'CA -- Ontario', 'CA -- Nova Scotia', 'CA -- Newfoundland And Labrador', 'CA -- New Brunswick','CA -- Newfoundland and Labrador')
  CA_WEST <- c('CA -- Alberta', 'CA -- British Columbia', 'CA -- Manitoba', 'CA -- Northwest Territories', 'CA -- Nunavut', 'CA -- Saskatchewan', 'CA -- Yukon Territory')
  US_Flordia <- c('US -- Florida')
  US_Midwest <- c('US -- Wisconsin', 'US -- Ohio', 'US -- Minnesota', 'US -- Michigan', 'US -- Indiana', 'US -- Illinois','US -- South Dakota', 'US -- Oklahoma', 'US -- North Dakota', 'US -- Nebraska','US -- Missouri', 'US -- Kansas', 'US -- Iowa')
  US_GulfCoast <- c('US -- Texas', 'US -- Mississippi', 'US -- Louisiana', 'US -- Alabama')
  US_Mid_Atlantic <- c('US -- West Virginia', 'US -- Virginia', 'US -- Pennsylvania', 'US -- Maryland', 'US -- Delaware', 'US -- District Of Columbia','US -- District of Columbia')
  US_New_England <- c('US -- Vermont', 'US -- Rhode Island', 'US -- New Hampshire', 'US -- Massachusetts', 'US -- Maine', 'US -- Connecticut')
  US_NorthWest <- c('US -- Washington', 'US -- Oregon', 'US -- Idaho', 'US -- Alaska','US -- Wyoming', 'US -- Montana')
  US_Southeast <- c('US -- Tennessee', 'US -- South Carolina', 'US -- North Carolina', 'US -- Kentucky', 'US -- Georgia', 'US -- Arkansas')
  US_Southwest <- c('US -- Utah', 'US -- New Mexico', 'US -- Nevada', 'US -- Hawaii', 'US -- Colorado', 'US -- California', 'US -- Arizona')
  US_NY_NJ <- c('US -- New Jersey', 'US -- New York')
  
  DelByGeo_df$NoGeoExist <- !grepl("-",DelByGeo_df$Geo)
  DelByGeo_df$Country <- str_trim(ifelse(grepl("-",DelByGeo_df$Geo),substr(DelByGeo_df$Geo,1,2),"unknown"))
  DelByGeo_df$State <- str_trim(ifelse(grepl("-",DelByGeo_df$Geo),substr(DelByGeo_df$Geo,7,nchar(DelByGeo_df$Geo)),"unknown"))
  
  Africa <- c('NG', 'DJ', 'CI', 'GH', 'SN', 'NA', 'TG', 'TN', 'DZ', 'SC', 'MU', 'KE', 'MA', 'BJ', 'MW', 'CD', 'ML', 'GM', 'MZ', 'TZ', 'AO', 'LY', 'GA', 'CM', 'UG', 'RE', 'BW', 'CV', 'TD', 'YT', 'SL', 'SD', 'ZW', 'GW', 'BI', 'GN', 'ZM', 'SZ', 'CG', 'RW', 'MR', 'CF', 'ET', 'SH', 'GQ', 'BF', 'SO', 'LS', 'NE', 'ST', 'ZA','ER','LR','KM')
  Europe <- c('DE', 'GR', 'CZ', 'MC', 'FR', 'IE', 'UA', 'BE', 'LV', 'PL', 'BG', 'NL', 'GI', 'ES', 'ME', 'RS', 'PT', 'HR', 'MT', 'AT', 'RO', 'SK', 'HU', 'CH', 'EE', 'LT', 'LU', 'BA', 'IM', 'BY', 'DK', 'AX', 'GG', 'AL', 'MD', 'LI', 'JE', 'SM', 'MK', 'IS', 'AD', 'VA','FO','GL','IT','SI','SJ','XK')
  FarEast <- c('JP', 'SG', 'IN', 'PK', 'CN', 'MY', 'TH', 'HK', 'ID', 'PH', 'VN', 'GE', 'TW', 'MO', 'BD', 'KZ', 'MN', 'BN', 'KH', 'BT', 'MM', 'IO', 'TM', 'TL', 'KG', 'LA', 'UZ', 'CX', 'TJ','KR')
  NorthAmericaInt <- c('MQ', 'PA', 'AG', 'BM', 'BS', 'VI', 'VC', 'PR', 'TT', 'VG', 'MX', 'DO', 'GD', 'KY', 'CR', 'DM', 'AW', 'GT', 'GP', 'BZ', 'SV', 'BB', 'KN', 'AI', 'JM', 'LC', 'MS', 'NI', 'CU', 'HT', 'TC', 'HN')
  Australiasia <- c('AU', 'NZ', 'NC', 'GU', 'PF', 'VU', 'FJ', 'PG', 'CK', 'MP', 'PW', 'TO', 'NU', 'SB', 'FM', 'KI', 'WF', 'TV', 'NR', 'WS', 'NF', 'AS','AF','AM','AZ')
  MiddleEast <- c('BH', 'CY', 'EG', 'IR', 'IQ', 'IL', 'JO', 'KW', 'LB', 'OM', 'QA', 'SA', 'SY', 'TR', 'AE', 'YE','PS')
  SouthAmerica <- c('EC', 'AR', 'VE', 'BR', 'CO', 'PE', 'UY', 'BO', 'CL', 'PY', 'SR', 'GY', 'GF', 'FK','BQ')
  RussiaScandinavia <- c('FI', 'NO', 'RU', 'SE')
  UK <- c('GB')
  
  
  DelByGeo_df$Area <- ifelse(DelByGeo_df$Geo %in% CA_EAST,"CA_EAST",
                            ifelse(DelByGeo_df$Geo %in% CA_WEST, "CA_WEST",
                                   ifelse(DelByGeo_df$Geo %in% US_Flordia, "US_Florida",
                                          # ifelse(DelByGeo_df$Geo %in% US_GreatLakes,"US_GreatLakes",
                                                 ifelse(DelByGeo_df$Geo %in% US_GulfCoast, "US_GulfCoast",
                                                        ifelse(DelByGeo_df$Geo %in% US_Mid_Atlantic, "US_Mid_Atlantic",
                                                               ifelse(DelByGeo_df$Geo %in% US_Midwest,"US_MId_West",
                                                                      ifelse(DelByGeo_df$Geo %in% US_New_England,"US_New_England",
                                                                             ifelse(DelByGeo_df$Geo %in% US_NorthWest, "US_NorthWest",
                                                                                    ifelse(DelByGeo_df$Geo %in% US_Southeast, "US_Southeast",
                                                                                           ifelse(DelByGeo_df$Geo %in% US_Southwest, "US_Southwest",
                                                                                              ifelse(DelByGeo_df$Geo %in% US_NY_NJ, "US_NY_NJ",
                                                                                                     
                                                                                                  ifelse(DelByGeo_df$Country %in% Africa, "Africa",
                                                                                                         ifelse(DelByGeo_df$Country %in% Europe, "Europe",
                                                                                                                ifelse(DelByGeo_df$Country %in% FarEast, "FarEast",
                                                                                                                       ifelse(DelByGeo_df$Country %in% NorthAmericaInt, "NorthAmericaInt",
                                                                                                                              ifelse(DelByGeo_df$Country %in% MiddleEast, "MiddleEast",
                                                                                                                                     ifelse(DelByGeo_df$Country %in% SouthAmerica, "SouthAmerica",
                                                                                                                                            ifelse(DelByGeo_df$Country %in% RussiaScandinavia, "RussiaScandinavia",
                                                                                                                                                   ifelse(DelByGeo_df$Country %in% Australiasia, "Australiasia",
                                                                                                                                                          ifelse(DelByGeo_df$Country %in% UK, "UK",
                                                                                                                                                                 ifelse(tolower(DelByGeo_df$Country) == 'unknown', "Other - unkown",
                                                                                                                                                                        "Other - international")))))))))))))))))))))
  
    return(DelByGeo_df)
}