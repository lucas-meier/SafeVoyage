

# load package
library(rvest)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)

register_google(key = "AIzaSyDDAsx0G3_qDx30jqbcurxEbvJXo_9UrdI", write = TRUE)



#Number of crimes by location in London boroughs

url_link = "https://www.finder.com/uk/london-crime-statistics"
browseURL(url_link)
crime_rates_per_boroughs_table = url_link  %>%
  read_html()  %>%  html_nodes(xpath='//*[@id="tab6066-1"]/table') %>%
  html_table()

crime_rates_per_boroughs_df = as.data.frame(crime_rates_per_boroughs_table)
head(crime_rates_per_boroughs_df)

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
  #LaddCircleMarkers(lng= boroughs_coord$lon, lat=boroughs_coord$lat, radius = as.numeric(crime_rates_per_boroughs_df$Crime.count)*5000,
                 #  popup = paste(crime_rates_per_boroughs_df$Borough.Name, crime_rates_per_boroughs_df$Crime.count, sep = "</br>"),
                 #  options = popupOptions(autoClose = FALSE))




  addCircleMarkers(lng=boroughs_coord$lon, lat=boroughs_coord$lat,
                   radius = crime_rates_per_boroughs_df$Crime.count) %>% 
  addPopups(boroughs_coord$lon[1], boroughs_coord$lat[1], popup_content[[6]], options = popupOptions(closeButton = F)) %>%
  addPopups(boroughs_coord$lon[2], boroughs_coord$lat[2], popup_content[[2]], options = popupOptions(closeButton = F)) %>%
  addPopups(boroughs_coord$lon[4], boroughs_coord$lat[4], popup_content[[4]], options = popupOptions(closeButton = F))%>% 
  addPopups(boroughs_coord$lon[5], boroughs_coord$lat[5], popup_content[[5]], options = popupOptions(closeButton = F))


