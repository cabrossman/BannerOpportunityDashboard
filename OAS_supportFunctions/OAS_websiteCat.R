
OAS_websiteCat <- function(site){
  website <- site
  website <- "NULL"

  for(o in 1:length(site)){
    if(grepl('www.boattrader.com.*',site[o],perl = TRUE)){
      website[o] <- 'BT'
    }
    else if(grepl('.*yacht.*',site[o],perl = TRUE)){
      website[o] <- 'YW'
    }
    else if(grepl('.*boats.com.*',site[o],perl = TRUE)){
      website[o] <- 'BC'
    }
    else if(grepl('boatwizard',site[o],perl = TRUE)){
      website[o] <- 'boatwizard'
    }
    else {
      website[o] <- 'other'
    }
  }
  return(website)
}
