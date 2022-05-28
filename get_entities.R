query <- "https://api.ioda.inetintel.cc.gatech.edu/v2/entities/query?relatedTo=country%2FVE"
fetch_data(query) -> entities



drop <- c("4481","4881","SA")

entities %>%
  mutate(name = ifelse(type == "asn",attrs$org,name))  %>% 
  mutate(attrs.ip_count = as.numeric(attrs$ip_count)) %>%
filter(!code %in% drop) -> entities



