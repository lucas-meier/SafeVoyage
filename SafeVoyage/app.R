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
    titlePanel("SafeVoyage - Keeping you safe during your travels"),
    tags$h5("Welcome to SafeVoyage, your place for all things safety while you travel."),
    tags$h5("Have fun playing around with our app and stay tuned for further updates!"),
    
    # Sidebar with all the different input possible 
    sidebarLayout(
        sidebarPanel(
          tags$h3("Where would you like to go?"),
          selectInput("selectcity","Access crime data for the following destinations: ", choices = c( " ", "London", "USA", "Global")),
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

        ###########################################
        
        #Global
        url_link = "https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate"
        crime_rates_per_global_city_table = url_link  %>%
            read_html()  %>%  html_nodes(xpath='//*[@id="UNODC"]') %>%
            html_table(fill = T)
        
        crime_rates_per_global_countries_df = as.data.frame(crime_rates_per_global_city_table)
        
        (crime_rates_per_global_countries_df)
        
        crime_rates_per_global_countries_df$Count = str_replace(string = gsub(x = crime_rates_per_global_countries_df$Count, pattern = "",
                                                                             replacement = ""), pattern = ",", ".") %>% as.numeric()
        
        
        global_coord = geocode(crime_rates_per_global_countries_df$Country..or.dependent.territory.subnational.area..etc..)
        
        # define pop up content
        popup_content = list()
        for(i in seq(nrow(crime_rates_per_global_countries_df))){
            popup_content[[i]] =  paste(sep = "<br/>",
                                        paste("<b>", crime_rates_per_global_countries_df$Country..or.dependent.territory.subnational.area..etc..[i]," </b>"),
                                        crime_rates_per_global_countries_df$Count[i])
        }
        
        
        ##########################################
        
        city <- reactiveVal(" ")
        
        observeEvent(input$run,{
            if(input$selectcity == "London"){
            output$map <- renderLeaflet({
                # plot map London
                leaflet(df) %>% addTiles() %>%
                    addCircleMarkers(lng= boroughs_coord$lon, lat=boroughs_coord$lat, radius = as.numeric(crime_rates_per_boroughs_df$Crime.count)*1.5,
                                     color = ifelse(crime_rates_per_boroughs_df$Crime.count >= 35, "darkred", ifelse(crime_rates_per_boroughs_df$Crime.count < 35 & crime_rates_per_boroughs_df$Crime.count >= 25, "darkorange", "darkgreen"))   
                    ) %>%
                    addPopups(boroughs_coord$lon, boroughs_coord$lat, paste(crime_rates_per_boroughs_df$Borough.Name, crime_rates_per_boroughs_df$Crime.count,
                                                                            sep = "</br>"), options = popupOptions(closeButton = F))
            
            
            })}
            else if(input$selectcity == "USA") {
                output$map <- renderLeaflet({
                    # plot map USA
                    leaflet(df) %>% addTiles() %>%
                        addCircleMarkers(lng= uscities_coord$lon, lat=uscities_coord$lat, radius = as.numeric(crime_rates_per_us_city_df$Violent.crime)*0.04,
                                         color = ifelse(crime_rates_per_us_city_df$Violent.crime >= 1500, "darkred", ifelse(crime_rates_per_us_city_df$Violent.crime < 1500 & crime_rates_per_us_city_df$Violent.crime >= 600, "darkorange", "darkgreen"))   
                        ) %>%
                        addPopups(uscities_coord$lon, uscities_coord$lat, paste(crime_rates_per_us_city_df$City, crime_rates_per_us_city_df$Violent.crime,
                                                                                sep = "</br>"), options = popupOptions(closeButton = F))
                    
                })
                
            }
            
            else if(input$selectcity == "Global") {
                output$map <- renderLeaflet({
                    # plot map USA
                    leaflet(df) %>% addTiles() %>%
                        addCircleMarkers(lng= global_coord$lon, lat=global_coord$lat, radius = as.numeric(crime_rates_per_global_countries_df$Count)*0.06,
                                         color = ifelse(crime_rates_per_global_countries_df$Count >= 700, "darkred", ifelse(crime_rates_per_global_countries_df$Count < 700 & crime_rates_per_global_countries_df$Count >= 200, "darkorange", "darkgreen"))   
                        ) %>%
                        addPopups(global_coord$lon, global_coord$lat, paste(crime_rates_per_global_countries_df$Country..or.dependent.territory.subnational.area..etc.., crime_rates_per_global_countries_df$Count,
                                                                                sep = "</br>"), options = popupOptions(closeButton = F))
                    
                })
                
            }
        })
        
        observeEvent(input$reset,{
            output$map <- renderLeaflet({
                NULL
            })
        })
        
       
       
       
##########################################################################################
        
    }


# Run the application 
shinyApp(ui = ui, server = server)
