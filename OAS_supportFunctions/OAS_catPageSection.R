OAS_catPageSection <- function(URL){
  #This section categorizes the page names into websites and sections based on the URL 
  section <- URL
  section <- 'NULL'

  for(i in 1:length(URL)){
    
    
    if(str_detect(URL[i],'detail')){
      section[i] = 'DT'
    } else if(str_detect(URL[i],'browse')){
      section[i] = 'BR'
    } else if(str_detect(URL[i],'adv')){
      section[i] = 'ADVSR'
    } else if(str_detect(URL[i],'search')){
      section[i] = 'SR'
    } else if(str_detect(URL[i],'page') || str_detect(URL[i],'.*com$')){
      section[i] = 'HOME'
    }
      else if(str_detect(URL[i],'www.boattrader.com/find/dealers/main.php')){
        section[i] = 'BR'
      }
      else if(str_detect(URL[i],'.*myt/listings.php')){
        section[i] = 'DT'
      }
      else if(str_detect(URL[i],'engine')){
        section[i] = 'ENG'
      }
      else if(str_detect(URL[i],'features')){
        section[i] = 'features'
      }
      else if(str_detect(URL[i],'insurance')){
        section[i] = 'insurance'
      }
      else if(str_detect(URL[i],'directory')){
        section[i] = 'RES'
      }
      else if(str_detect(URL[i],'fsbo')){
        section[i] = 'RES'
      }
      else if(str_detect(URL[i],'pbs')){
        section[i] = 'RES'
      }
      else if(str_detect(URL[i],'transport')){
        section[i] = 'transport'
      } else {section[i] = 'OTHER'}
    
  }
  return(section)
}



