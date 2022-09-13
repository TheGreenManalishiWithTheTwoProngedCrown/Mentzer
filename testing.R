library(jsonlite)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(httr)
httr::set_config(config(ssl_verifypeer=FALSE))


unix_date <- function(x){
  dttime <- as_datetime(as.POSIXct(x,origin = "1970-01-01"))
  return(dttime)
  }


unix_from_date <- function(x){
  as.numeric(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"))-14400
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
    100*value/max(value,na.rm = TRUE))
}

get_code_from_name <- function(name,data){
  return(data[data$name == name,]$code)
}

ma <- function(x, n = 2){
  stats::filter(x, rep(1 / n, n), sides = 2)
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
  
extract_df <- function(region_input=NULL,date_list,normalize_bool= FALSE, moving_average = FALSE, isp_req = NULL,vnzla = FALSE,datatype="Active Probing"){
  from <- unix_from_date(date_list[1])
  until <- unix_from_date(date_list[2])
  
  datatype <- case_when(
    datatype == "Active Probing" ~ "ping-slash24",
    datatype == "BGP" ~ "bgp",
    datatype == "Telescope" ~ "merit-nt"
  )
  

  if(!is.null(region_input)){
  codes <- lapply(region_input,get_code_from_name,entities)
  url <-create_url("region",codes,from,until,datatype)
  dataframe <- test_func(fetch_data(url))
  }
  
  if (!is.null(isp_req)) {
    url_isp <- create_url("asn",lapply(isp_req,get_code_from_name,entities),from,until,datatype)
    dataframe_isp <- test_func(fetch_data(url_isp))
   dataframe_isp <-  dataframe_isp %>% 
      left_join(select(entities,code,name), by = c("entityCode" = "code")) 
   
dataframe_isp <- dataframe_isp %>% mutate(entityName = name) %>% select(-name)

    if(is.null(region_input)){
      dataframe <- dataframe_isp
     
    }
    
    if ((!is.null(isp_req) && !is.null(region_input))) {
      dataframe <- rbind(dataframe,dataframe_isp)
    }
  }
  
  if(vnzla == TRUE){
    url_vnzla <- create_url("country","VE",from,until,datatype)
    dataframe_vnzla <- test_func(fetch_data(url_vnzla))
    if(is.null(region_input) && is.null(isp_req)){
      dataframe <- dataframe_vnzla
    }else{

    dataframe <- rbind(dataframe,dataframe_vnzla)
    }
  }
  
  
  if(normalize_bool){
    dataframe %>% 
      mutate(values = normalize(values)) -> dataframe
  }
  
  
  if(moving_average){
    dataframe %>% 
      mutate(values = ma(values)) -> dataframe
    
  }
  return(dataframe)
}


formatter_number <- function(x){
  x <- as.numeric(x)
  case_when(
    x < 1e3 ~ as.character(round(x,1)),
    x < 1e6 ~ paste0(as.character(round(x/1e3,1)), "K"),
    x < 1e9 ~ paste0(as.character(round(x/1e6,1)), "M")
  )
}
