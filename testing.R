library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)



unix_date <- function(x){
  dttime <- as_datetime(as.POSIXct(x,origin = "1970-01-01"))
  return(dttime)
  }


unix_from_date <- function(x){
  as.numeric(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"))
}


fetch_data <- function(url){
  raw <- httr::GET(url)
  content <- httr::content(raw,as="text")
  return(fromJSON(content)$data)
}



create_url <- function(type,code,from,until,datasource){
  header = "http://api.ioda.inetintel.cc.gatech.edu/v2/signals/raw/"
  

  if (length(code) > 1) {
    code <- paste(code, collapse= "%2C")
  }
  url <- paste0(header,type,"/",code,"?from=",as.character(from),"&until=",as.character(until),"&datasource=",datasource)
  return(url)
}



normalize <- function(value){
  return(
  value/max(value,na.rm = TRUE))
}


get_code_from_name <- function(name,data){
  return(data[data$name == name,]$code)
}

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

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


extract_df <- function(region_input,date_list,normalize_bool= FALSE){
  print(region_input)
  print(typeof(region_input))
  codes <- lapply(region_input,get_code_from_name,entities)
  print(codes)
  from <- unix_from_date(date_list[1])
  until <- unix_from_date(date_list[2])
  url <-create_url("region",codes,from,until,"ping-slash24")
  dataframe <- test_func(fetch_data(url))
  
  if(normalize_bool){
    dataframe %>% 
      mutate(values = ma(values)) -> dataframe
    
    
  }
  
  return(dataframe)
}
