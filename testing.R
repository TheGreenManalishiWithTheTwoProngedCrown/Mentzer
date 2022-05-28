library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)



unix_date <- function(x){
  dttime <- as_datetime(as.POSIXct(x,origin = "1970-01-01"))
  return(dttime)
  }


fetch_data <- function(url){
  raw <- httr::GET(url)
  content <- httr::content(raw,as="text")
  return(fromJSON(content)$data)
}



create_url <- function(type,code,from,until,datasource){
  header = "https://api.ioda.inetintel.cc.gatech.edu/v2/signals/raw/"
  url <- paste0(header,type,"/",code,"?from=",as.character(from),"&until=",as.character(until),"&datasource=",datasource)
  return(url)
}



normalize <- function(value){
  return(
    (value)/(max(value))
  )
}


get_code_from_name <- function(name,data){
  return(data[data$name == name,]$code)
}

test_func <- function(alist){
  
  
lapply(alist, unnest,values) -> temp
  Reduce(full_join,temp) -> temp 
  from <- unique(temp$from)
  until <- unique(temp$until)
  step <- unique(temp$step)
  timeperiod <- seq(from,until -step,step)
  temp %>% 
    group_by(entityCode) %>% 
    mutate(date = timeperiod) %>% 
    mutate(date = unix_date(date)) %>% 
    select(-from,-until,-step,-nativeStep)
    
}

