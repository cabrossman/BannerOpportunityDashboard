OAS_catFullDomainName <- function(URL){
    
    #categorize site of page
    URLCat <- URL
    URLCat <- "NULL"
    for(l in 1:length(URL)){
      if(grepl("^m",URL[l])){
        URLCat[l] <- 'mobile'
      } else if(grepl("^www.au.boats",URL[l])) {
        URLCat[l] <- 'au'
      } else if(grepl(".*com$",URL[l])) {
        URLCat[l] <- 'domestic'
      }
      else if(grepl("^www.boats.com/en/",URL[l])) {
        URLCat[l] <- 'domestic'
      }
      else if(grepl("^www.boattrader.com",URL[l])) {
        URLCat[l] <- 'domestic'
      }
      else if(grepl("^www.ca.boats",URL[l])) {
        URLCat[l] <- 'ca'
      }
      else if(grepl("^www.de.boats",URL[l])) {
        URLCat[l] <- 'de'
      }
      else if(grepl("^www.es.boats",URL[l])) {
        URLCat[l] <- 'es'
      } 
      else if(grepl("^www.fr.boats",URL[l])) {
        URLCat[l] <- 'fr'
      } 
      else if(grepl("^www.it.boats",URL[l])) {
        URLCat[l] <- 'it'
      } 
      else if(grepl("www.nl.boats",URL[l])) {
        URLCat[l] <- 'nl'
      } 
      else if(grepl("^www.uk.boats",URL[l])) {
        URLCat[l] <- 'uk'
      } 
      else if(grepl("^www.us.boats",URL[l])) {
        URLCat[l] <- 'domestic'
      } 
      else if(grepl("^www.yachtworld.com/au",URL[l])) {
        URLCat[l] <- 'au'
      } 
      else if(grepl("^www.yachtworld.com/de",URL[l])) {
        URLCat[l] <- 'de'
      } 
      else if(grepl("^www.yachtworld.com/dk",URL[l])) {
        URLCat[l] <- 'dk'
      } 
      else if(grepl("^www.yachtworld.com/e/",URL[l])) {
        URLCat[l] <- 'e'
      } 
      else if(grepl("^www.yachtworld.com/en",URL[l])) {
        URLCat[l] <- 'domestic'
      } 
      else if(grepl("^www.yachtworld.com/es",URL[l])) {
        URLCat[l] <- 'es'
      } 
      else if(grepl("^www.yachtworld.com/fi",URL[l])) {
        URLCat[l] <- 'fi'
      } 
      else if(grepl("^www.yachtworld.com/fr",URL[l])) {
        URLCat[l] <- 'fr'
      } 
      else if(grepl("^www.yachtworld.com/gb",URL[l])) {
        URLCat[l] <- 'gb'
      } 
      else if(grepl("^www.yachtworld.com/it",URL[l])) {
        URLCat[l] <- 'it'
      } 
      else if(grepl("^www.yachtworld.com/nl",URL[l])) {
        URLCat[l] <- 'nl'
      } 
      else if(grepl("^www.yachtworld.com/no",URL[l])) {
        URLCat[l] <- 'no'
      } 
      else if(grepl("^www.yachtworld.com/ru",URL[l])) {
        URLCat[l] <- 'ru'
      } 
      else if(grepl("^www.yachtworld.com/t/",URL[l])) {
        URLCat[l] <- 't'
      } 
      else if(grepl("^www.yachtworld.com/uk",URL[l])) {
        URLCat[l] <- 'uk'
      } 
      else if(grepl("^www.yachtworldcharters.com",URL[l])) {
        URLCat[l] <- 'ywchart'
      } 
      else if(grepl("^www.yachtworld.com/se/",URL[l])) {
        URLCat[l] <- 'se'
      } 
      else if(grepl("^www.boats.com/en",URL[l])) {
        URLCat[l] <- 'domestic'
      } 
      else if(grepl("^www.boats.com/e/",URL[l])) {
        URLCat[l] <- 'e'
      } 
      else if(grepl("^www.yachtworld.com/us/",URL[l])) {
        URLCat[l] <- 'domestic'
      } 
      else if(grepl("^www.yachtworld.com/n/",URL[l])) {
        URLCat[l] <- 'n'
      } 
      else if(grepl("^www.yachtworld.com/s/",URL[l])) {
        URLCat[l] <- 's'
      } 
      else if(grepl("^www.yachtworld.com/dn/",URL[l])) {
        URLCat[l] <- 's'
      } 
      else if(grepl("^www.boats.com/gb/",URL[l])) {
        URLCat[l] <- 'gb'
      } 
      else if(grepl("^www.boats.com/de/",URL[l])) {
        URLCat[l] <- 'de'
      } 
      else if(grepl("^www.yachtworld.com/sv/",URL[l])) {
        URLCat[l] <- 'sv'
      } 
      else if(grepl("^www.boats.com/au/",URL[l])) {
        URLCat[l] <- 'au'
      } 
      else if(grepl("^www.yachtworld.com/r/",URL[l])) {
        URLCat[l] <- 'r'
      } 
      else if(grepl("^www.boats.com/ca/",URL[l])) {
        URLCat[l] <- 'ca'
      } 
      else {
        URLCat[l] <- 'other'
      }
    }
  return(URLCat)
}
