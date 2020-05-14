#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)


# Define UI for application
ui <- fluidPage(theme = shinytheme("sandstone"),
                
    # Application title
    titlePanel("SafeVoyage"),

    # Sidebar with all the different input possible 
    sidebarLayout(
        sidebarPanel(
          tags$h3("Hello ! Where would you like to go ?"),
          selectInput("selectcity","Select a city : ", choices = c( "","London")),
          actionButton("run","Search",icon = NULL, width = '100px'),
          actionButton("reset", "Reset", icon = NULL, width ='100px'),
          hr()
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map", width = "100%", height = "400px")
        )
    )
)

# Define server logic required to draw the output
server <- function(input, output) {
 
    
    ###################  OUR CODE  ####################
        
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
        
        # Number of crimes by location in London boroughs
        url_link = "https://www.finder.com/uk/london-crime-statistics"
        crime_rates_per_boroughs_table = url_link  %>%
            read_html()  %>%  html_nodes(xpath='//*[@id="tab6066-1"]/table') %>%
            html_table()
        
        crime_rates_per_boroughs_df = as.data.frame(crime_rates_per_boroughs_table)
        
        crime_rates_per_boroughs_df$Crime.count = str_replace(string = gsub(x = crime_rates_per_boroughs_df$Crime.count, pattern = "",
                                                                            replacement = ""), pattern = ",", ".") %>% as.numeric()
        
        boroughs_coord = geocode(paste("London district ", crime_rates_per_boroughs_df$Borough.Name))
        
        # define pop up content
        popup_content = list()
        for(i in seq(nrow(crime_rates_per_boroughs_df))){
            popup_content[[i]] =  paste(sep = "<br/>",
                                        paste("<b>", crime_rates_per_boroughs_df$Borough.Name[i]," </b>"),
                                        crime_rates_per_boroughs_df$Crime.count[i])
        }
        
        ######################################################
        
        #USA
        url_link = "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate"
        crime_rates_per_us_city_table = url_link  %>%
            read_html()  %>%  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
            html_table(fill = T)
        
        crime_rates_per_us_city_df = as.data.frame(crime_rates_per_us_city_table)
        
        crime_rates_per_us_city_df$Violent.crime = str_replace(string = gsub(x = crime_rates_per_us_city_df$Violent.crime, pattern = "",
                                                                            replacement = ""), pattern = ",", ".") %>% as.numeric()
        
        
        uscities_coord = geocode(paste("USA ", crime_rates_per_us_city_df$City))
        
        # define pop up content
        popup_content = list()
        for(i in seq(nrow(crime_rates_per_us_city_df))){
            popup_content[[i]] =  paste(sep = "<br/>",
                                        paste("<b>", crime_rates_per_us_city_df$City[i]," </b>"),
                                        crime_rates_per_us_city_df$Violent.crime[i])
        }
        
        (crime_rates_per_us_city_df)
        
        ###########################################
        
        
        
        
       
        output$map <- renderLeaflet({
            
            # plot map London
            leaflet(df) %>% addTiles() %>%
                addCircleMarkers(lng= boroughs_coord$lon, lat=boroughs_coord$lat, radius = as.numeric(crime_rates_per_boroughs_df$Crime.count)*1.5,
                                 color = ifelse(crime_rates_per_boroughs_df$Crime.count >= 35, "darkred", ifelse(crime_rates_per_boroughs_df$Crime.count < 35 & crime_rates_per_boroughs_df$Crime.count >= 25, "darkorange", "darkgreen"))   
                ) %>%
                addPopups(boroughs_coord$lon, boroughs_coord$lat, paste(crime_rates_per_boroughs_df$Borough.Name, crime_rates_per_boroughs_df$Crime.count,
                                                                        sep = "</br>"), options = popupOptions(closeButton = F))
            
            # plot map USA
            leaflet(df) %>% addTiles() %>%
                addCircleMarkers(lng= uscities_coord$lon, lat=uscities_coord$lat, radius = as.numeric(crime_rates_per_us_city_df$Violent.crime)*0.04,
                                 color = ifelse(crime_rates_per_us_city_df$Violent.crime >= 35*0.04, "darkred", ifelse(crime_rates_per_us_city_df$Violent.crime < 35*0.04 & crime_rates_per_us_city_df$Violent.crime >= 25*0.04, "darkorange", "darkgreen"))   
                ) %>%
                addPopups(uscities_coord$lon, uscities_coord$lat, paste(crime_rates_per_us_city_df$City, crime_rates_per_us_city_df$Violent.crime,
                                                                        sep = "</br>"), options = popupOptions(closeButton = F))
        
        })
##########################################################################################
        
    }


# Run the application 
shinyApp(ui = ui, server = server)
