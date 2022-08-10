venequia <- geojsonio::geojson_read("geojson/venezuela.geojson",what = 'sp')

until <- unix_from_date(now() + hours(8))
from <- unix_from_date(now() - days(10) + hours(4)) #VERIFICAR ESO EL DESPLAZAMIENTO, LA FUNCION UNIX_... PARECE TENER UN OFFSET


url <- paste("http://api.ioda.inetintel.cc.gatech.edu/v2/outages/events?from=",
             format(from,scientific = FALSE)
             ,"&until=",
             format(until,scientific = FALSE),
             "&includeAlertsors=true&limit=2000&relatedTo=country%2FVE&overall=false", 
             sep = "") 

fetch_data(url) -> outages_raw
## Filtramos para solo tener las regiones




outag_isp_score <- outages_raw %>% 
  separate(location,c("type","code"),sep = "/") %>% 
  filter(type == "asn") %>% 
  select(code,score)

outages <- outages_raw %>% 
  separate(location,c("type","code"),sep = "/") %>% 
  filter(type == "region") %>%
  mutate( ESTADO = location_name) %>% 
  group_by(ESTADO) %>%  
  top_n(1,score) %>% 
  distinct(ESTADO,.keep_all = TRUE) %>% 
  mutate(LABEL = paste("<strong>",ESTADO,"</strong>","-",as.integer(score))) 

outage_regions <- outages %>% 
  select(location_name)

outages <- outages %>% 
  select(ESTADO,score,LABEL)
  # right_join(entities, by = c("location_name" = "name"))
  # 

#outages$score[is.na(outages$score)] <- 0


if(dim(outages)[1] != 0){
  outages$ESTADO <-  toupper(iconv(outages$ESTADO,to = 'ASCII//TRANSLIT'))
  
  venequia <- sp::merge(venequia, outages, by ="ESTADO",all.x = FALSE)
}

url <- paste("https://api.ioda.inetintel.cc.gatech.edu/v2/outages/alerts?from=",
             format(from,scientific = FALSE)
             ,"&until=",
             format(until,scientific = FALSE),
             "&includeAlertsors=true&limit=2000&relatedTo=country%2FVE&overall=false", 
             sep = "") 


fetch_data(url) -> alertas
providers <- alertas %>% 
  unnest(cols = c(entity)) %>% 
  unnest(cols = c(attrs), names_repair = 'unique') %>% 
  filter(type == "asn") %>% 
  group_by(org) %>% 
  slice_max(value) %>% 
  select(org,datasource,fqid,ip_count,time,level,condition,value,historyValue) %>% 
  separate(fqid,c("type","code"),sep = '\\.') %>% 
  left_join(outag_isp_score, by= "code") %>% 
  mutate(score = formatter_number(score),
         ip_count = formatter_number(ip_count),
         Codigo = paste0("AS",code)) 
 

rm(alertas,url,from,until,outages_raw,outag_isp_score)
