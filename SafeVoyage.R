
# load package
library(rvest)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(tidyverse)
library(jtools)

register_google(key = "AIzaSyDDAsx0G3_qDx30jqbcurxEbvJXo_9UrdI", write = TRUE)

#Number of crimes by location in London boroughs

url_link = "https://www.finder.com/uk/london-crime-statistics"
browseURL(url_link)
crime_rates_per_boroughs_table = url_link  %>%
  read_html()  %>%  html_nodes(xpath='//*[@id="tab6066-1"]/table') %>%
  html_table()

crime_rates_per_boroughs_df = as.data.frame(crime_rates_per_boroughs_table)
#head(crime_rates_per_boroughs_df)
crime_rates_per_boroughs_df$Crime.count = str_replace(string = gsub(x = crime_rates_per_boroughs_df$Crime.count, pattern = "",
                                                                    replacement = ""), pattern = ",", ".") %>% as.numeric()

boroughs_coord = geocode(paste("London district", crime_rates_per_boroughs_df$Borough.Name))

# define pop up content
popup_content = list()
for(i in seq(nrow(crime_rates_per_boroughs_df))){
  popup_content[[i]] =  paste(sep = "<br/>",
                              paste("<b>", crime_rates_per_boroughs_df$Borough.Name[i]," </b>"),
                              crime_rates_per_boroughs_df$Crime.count[i])
}

# plot map
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(lng= boroughs_coord$lon, lat=boroughs_coord$lat, radius = as.numeric(crime_rates_per_boroughs_df$Crime.count)*1.5,
                   color = ifelse(crime_rates_per_boroughs_df$Crime.count >= 35, "darkred", ifelse(crime_rates_per_boroughs_df$Crime.count < 35 & crime_rates_per_boroughs_df$Crime.count >= 25, "darkorange", "darkgreen"))   
                   ) %>%
  addPopups(boroughs_coord$lon, boroughs_coord$lat, paste(crime_rates_per_boroughs_df$Borough.Name, crime_rates_per_boroughs_df$Crime.count,
                                                          sep = "</br>"), options = popupOptions(closeButton = F))
