


#Number of crimes by location in London boroughs

url_link = "https://www.finder.com/uk/london-crime-statistics"
browseURL(url_link)
crime_rates_per_boroughs_table = url_link  %>%
  read_html()  %>%  html_nodes(xpath='//*[@id="tab6066-1"]/table') %>%
  html_table()

crime_rates_per_boroughs_df = as.data.frame(crime_rates_per_boroughs_table)
head(crime_rates_per_boroughs_df)