
until <- unix_from_date(now() + hours(4))
from <- unix_from_date(now() - days(1) + hours(4)) #VERIFICAR ESO EL DESPLAZAMIENTO, LA FUNCION UNIX_... PARECE TENER UN OFFSET


url <- paste("https://api.ioda.inetintel.cc.gatech.edu/v2/outages/events?from=",
             format(from,scientific = FALSE)
             ,"&until=",
             format(until,scientific = FALSE),
             "&includeAlerts=true&limit=2000&relatedTo=country%2FVE&overall=false", 
             sep = "") 

fetch_data(url) -> outages

## Filtramos para solo tener las regiones

outages <- outages %>% 
  separate(location,c("type","code"),sep = "/") %>% 
  filter(type == "region") %>% 
  right_join(entities, by = c("location_name" = "name"))


outages$score[is.na(outages$score)] <- 0

outages$location_name <-  toupper(iconv(outages$location_name,to = 'ASCII//TRANSLIT'))


