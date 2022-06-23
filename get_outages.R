venequia <- geojsonio::geojson_read("geojson/venezuela.geojson",what = 'sp')

until <- unix_from_date(now() + hours(4))
from <- unix_from_date(now() - days(2) + hours(4)) #VERIFICAR ESO EL DESPLAZAMIENTO, LA FUNCION UNIX_... PARECE TENER UN OFFSET


url <- paste("https://api.ioda.inetintel.cc.gatech.edu/v2/outages/events?from=",
             format(from,scientific = FALSE)
             ,"&until=",
             format(until,scientific = FALSE),
             "&includeAlertsors=true&limit=2000&relatedTo=country%2FVE&overall=false", 
             sep = "") 

fetch_data(url) -> outages

## Filtramos para solo tener las regiones

outages <- outages %>% 
  separate(location,c("type","code"),sep = "/") %>% 
  filter(type == "region") %>%
  rename( ESTADO = location_name) %>% 
  group_by(ESTADO) %>%  
  top_n(1,start) %>% 
  mutate(LABEL = paste("<strong>",ESTADO,"</strong>","-",as.integer(score))) %>% 
  select(ESTADO,score,LABEL)
  # right_join(entities, by = c("location_name" = "name"))
  # 

#outages$score[is.na(outages$score)] <- 0

outages$ESTADO <-  toupper(iconv(outages$ESTADO,to = 'ASCII//TRANSLIT'))

venequia <- sp::merge(venequia, outages, by ="ESTADO",all.x = FALSE)
