OAS_listMarineSites <- function(){

  #get list of available sites, filter list for just yachtworld, BT & BC, filter based on non interested YW,BT,BC
  my_credentials <- oasAuth()
  sites <- oas_list(credentials=my_credentials, request_type='Site')
  sites <- sites[grepl("yacht|boat",sites$Id),]
  remove <- c('boattraderonline', 'reskin-qa.boats.com', 'www.boatwizard.com-SecureSite', 'www.yachtworldfoundation.com')
  sites <- sites[!(sites$Id %in% remove),]
  return(sites)
}