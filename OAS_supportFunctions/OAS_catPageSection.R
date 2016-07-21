OAS_catPageSection <- function(URL){
  
  
  
  
  adv_search <- c('www.yachtworld.com/en/boatsearch.html', 'www.boattrader.com/find/search.php', 'www.us.boats.com/advsearch.html', 'www.yachtworld.com/au/boatsearch.html', 'www.yachtworld.com/au/directory.html', 'www.yachtworld.com/de/boatsearch.html', 'www.yachtworld.com/de/directory.html', 'www.yachtworld.com/dk/boatsearch.html', 'www.yachtworld.com/dk/directory.html', 'www.yachtworld.com/es/boatsearch.html', 'www.yachtworld.com/es/directory.html', 'www.yachtworld.com/fi/boatsearch.html', 'www.yachtworld.com/fi/directory.html', 'www.yachtworld.com/fr/boatsearch.html', 'www.yachtworld.com/fr/directory.html', 'www.yachtworld.com/gb/boatsearch.html', 'www.yachtworld.com/gb/directory.html', 'www.yachtworld.com/it/boatsearch.html', 'www.yachtworld.com/it/directory.html', 'www.yachtworld.com/nl/boatsearch.html', 'www.yachtworld.com/nl/directory.html', 'www.yachtworld.com/no/boatsearch.html', 'www.yachtworld.com/no/directory.html', 'www.yachtworld.com/ru/boatsearch.html', 'www.yachtworld.com/ru/directory.html', 'www.yachtworld.com/se/boatsearch.html', 'www.yachtworld.com/se/directory.html', 'www.au.boats.com/advsearch.html', 'www.ca.boats.com/advsearch.html', 'www.de.boats.com/advsearch.html', 'www.es.boats.com/advsearch.html', 'www.fr.boats.com/advsearch.html', 'www.it.boats.com/advsearch.html', 'www.nl.boats.com/advsearch.html', 'www.uk.boats.com/advsearch.html')
  
  
  
  
  #This section categorizes the page names into websites and sections based on the URL 
  section <- URL
  section <- 'NULL'

  for(i in 1:length(URL)){
    
   
    
    
    if(URL[i] %in% adv_search){
      section[i] = 'ADVSR'
    } else if(str_detect(URL[i],'browse')){
      section[i] = 'BR'
    } else if(str_detect(URL[i],'detail')){
      section[i] = 'DT'
    } else if(str_detect(URL[i],'/resources')){
      section[i] = 'RES'
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
        section[i] = 'FEATURES'
      }
      else if(str_detect(URL[i],'insurance')){
        section[i] = 'INSURANCE'
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
        section[i] = 'TRANSPORT'
      } else {section[i] = 'OTHER'}
    
  }
  return(section)
}



